import requests
import re
import base64
import random
import itertools

get_unicode = re.compile("UnicodeDecodeError: 'utf-8' codec can't decode byte 0x(?P<hex>[0-9a-f]{2}) in position (?P<position>[0-9]+):")

def xor_at(bs, idx, val):
  before = bytearray(bs[:idx])
  after = bytearray(bs[idx+1:])
  changed = bytearray([bs[idx]^val])
  return before + changed + after

def do_login(username):
  s = requests.session()
  main_page = s.get("http://pad_practice.thenixuchallenge.com/")
  csrf = re.search("<input[^>]+id=\"csrf_token\"[^>]+value=\"(?P<csrf>[^\"]+)\"", main_page.text)
  login = s.post("http://pad_practice.thenixuchallenge.com/login", data={"csrf_token": csrf.group("csrf"), "name": username.decode("utf-8"), "submit": "Submit"})
  return (login, s)

def get_login_cookie(username):
  (login, s) = do_login(username)
  try:
    login_cookie = base64.b64decode(requests.utils.dict_from_cookiejar(s.cookies)["login"])
    return (login_cookie, s)
  except KeyError:
    return None

def get_block_and_iv_for(data):
  if len(data) != 16:
    raise ValueError("Preblock not fetchable for " + str(len(data)) + " bytes.")
  result = get_login_cookie(b"X" * 18)
  (cookie, _) = result
  return bytes(map(lambda a, b: a ^ b ^ ord('X'), cookie[16:32], data)) + cookie[32:48]

def find_decryption_for(blocks):
  s = requests.session()
  block_before = bytes([0]*16)
  if len(blocks) % 16 != 0:
    raise ValueError("Can't decrypt encryptions of non-16 multiple bytes")
  for selected in range(1*16, len(blocks), 16):
    work_with = blocks[selected:selected+16]
    fixed = [False]*16
    for changed in range(0, 16):
      if fixed[changed]:
        yield chr(ord('[') ^ block_before[changed] ^ blocks[changed + selected - 16])
      else:
        for i in range(0, 256):
            test_cookie = xor_at(block_before, changed, i) + work_with
    
            test_cookie_b64 = base64.b64encode(test_cookie).decode("utf-8")
          
            logged_in = s.get("http://pad_practice.thenixuchallenge.com/", cookies={"login": test_cookie_b64})
    
            result = get_unicode.search(logged_in.text)
            if logged_in.status_code == 200 and result:
              pos = int(result.group("position"))
              block_before = xor_at(block_before, pos, int(result.group("hex"), 16) ^ test_cookie[pos] ^ ord('[') ^ block_before[pos])
              fixed = fixed[:pos] + [True] + fixed[pos+1:]
              if pos == changed:
                yield chr(ord('[') ^ block_before[changed] ^ blocks[changed+selected-16])
                break
        else:
          raise RuntimeError("Unknown error when finding preblock")

def modify_preblock_for(data, blocks):
  if len(blocks) < 32 or len(blocks) % 16 != 0:
    raise ValueError("Can't modify the preblock for not divisible by 16 and less than two blocks")
  current_decryption = itertools.islice(find_decryption_for(bytes([0]*16) + blocks), 16)
  return bytes(map(lambda a, b: ord(a) ^ b, current_decryption, data)) + blocks

def create_crypto_data(data):
  if len(data) % 16 != 0:
    raise ValueError("Data needs to be a multiple of 16 bytes.")
  current = get_block_and_iv_for(data[-16:])
  for next_block_at in range(len(data) - 32, -1, -16):
    current = modify_preblock_for(data[next_block_at:next_block_at+16], current)
  return current

def create_padded_crypto_data(data):
  data2 = data + b'\0' * (16 - len(data) % 16)
  return create_crypto_data(data2)




test_crypto = create_padded_crypto_data(b'{"username": "admin", "giveFlag": true}')
print(test_crypto)
s = requests.session()
test_crypto_b64 = base64.b64encode(test_crypto).decode("utf-8")
for _ in range(10):
  result = s.get("http://pad_practice.thenixuchallenge.com/", cookies={"login": test_crypto_b64})
  print(result.status_code, result.text)
  if result.status_code == 200:
    break
