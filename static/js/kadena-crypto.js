(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.lib = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
(function (Buffer){
const Module = require('../lib.js')

const {validateBuffer, validateString, validateArray} = require("../utils/validation")

function blake2b(input, outputLen) {
  validateBuffer(input)

  const inputLen = input.length
  const inputArrPtr = Module._malloc(inputLen)
  const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)
  const outputArrPtr = Module._malloc(outputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, outputLen)

  inputArr.set(input)

  Module._emscripten_blake2b(inputArrPtr, inputLen, outputArrPtr, outputLen)

  Module._free(inputArrPtr)
  Module._free(outputArrPtr)

  return Buffer.from(outputArr)
}

function hmac_sha512(initKey, inputs) {
  validateBuffer(initKey)
  validateArray(inputs)
  inputs.map(validateBuffer)

  const ctxLen = Module._emscripten_size_of_hmac_sha512_ctx()
  const ctxArrPtr = Module._malloc(ctxLen)
  const ctxArr = new Uint8Array(Module.HEAPU8.buffer, ctxArrPtr, ctxLen)

  const initKeyLen = initKey.length
  const initKeyArrPtr = Module._malloc(initKeyLen)
  const initKeyArr = new Uint8Array(Module.HEAPU8.buffer, initKeyArrPtr, initKeyLen)

  const outputLen = 64
  const outputArrPtr = Module._malloc(outputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, outputLen)

  initKeyArr.set(initKey)

  Module._emscripten_hmac_sha512_init(ctxArrPtr, initKeyArrPtr, initKeyLen)

  for (let i = 0; i < inputs.length; i++) {
    const inputLen = inputs[i].length
    const inputArrPtr = Module._malloc(inputLen)
    const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)

    inputArr.set(inputs[i])

    Module._emscripten_hmac_sha512_update(ctxArrPtr, inputArrPtr, inputLen)

    Module._free(inputArrPtr)
  }

  Module._emscripten_hmac_sha512_final(ctxArrPtr, outputArrPtr)

  Module._free(initKeyArrPtr)
  Module._free(ctxArrPtr)
  Module._free(outputArrPtr)

  return Buffer.from(outputArr)
}


function chacha20poly1305Encrypt(input, key, nonce) {
  validateBuffer(input)
  validateBuffer(key, 32)
  validateBuffer(nonce, 12)

  const inputLen = input.length
  const inputArrPtr = Module._malloc(inputLen)
  const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)

  const keyLen = key.length
  const keyArrPtr = Module._malloc(keyLen)
  const keyArr = new Uint8Array(Module.HEAPU8.buffer, keyArrPtr, keyLen)

  const nonceLen = nonce.length
  const nonceArrPtr = Module._malloc(nonceLen)
  const nonceArr = new Uint8Array(Module.HEAPU8.buffer, nonceArrPtr, nonceLen)

  const tagLen = 16
  const outputLen = inputLen + tagLen
  const outputArrPtr = Module._malloc(outputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, outputLen)

  inputArr.set(input)
  keyArr.set(key)
  nonceArr.set(nonce)

  const resultCode = Module._emscripten_chacha20poly1305_enc(keyArrPtr, nonceArrPtr, inputArrPtr, inputLen, outputArrPtr, outputArrPtr + inputLen, tagLen, 1)

  Module._free(inputArrPtr)
  Module._free(keyArrPtr)
  Module._free(nonceArrPtr)
  Module._free(outputArrPtr)

  if (resultCode !== 0) {
    throw Error('chacha20poly1305 encryption has failed!')
  }

  return Buffer.from(outputArr)
}

function chacha20poly1305Decrypt(input, key, nonce) {
  validateBuffer(input)
  validateBuffer(key, 32)
  validateBuffer(nonce, 12)

  // extract tag from input
  const tagLen = 16
  const tag = input.slice(input.length - tagLen, input.length)
  input = input.slice(0, input.length - tagLen)

  const inputLen = input.length
  const inputArrPtr = Module._malloc(inputLen)
  const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)

  const tagArrPtr = Module._malloc(tagLen)
  const tagArr = new Uint8Array(Module.HEAPU8.buffer, tagArrPtr, tagLen)

  const keyLen = key.length
  const keyArrPtr = Module._malloc(keyLen)
  const keyArr = new Uint8Array(Module.HEAPU8.buffer, keyArrPtr, keyLen)

  const nonceLen = nonce.length
  const nonceArrPtr = Module._malloc(nonceLen)
  const nonceArr = new Uint8Array(Module.HEAPU8.buffer, nonceArrPtr, nonceLen)

  const outputLen = inputLen
  const outputArrPtr = Module._malloc(outputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, outputLen)

  inputArr.set(input)
  tagArr.set(tag)
  keyArr.set(key)
  nonceArr.set(nonce)

  const resultCode = Module._emscripten_chacha20poly1305_enc(keyArrPtr, nonceArrPtr, inputArrPtr, inputLen, outputArrPtr, tagArrPtr, tagLen, 0)

  Module._free(inputArrPtr)
  Module._free(keyArrPtr)
  Module._free(nonceArrPtr)
  Module._free(outputArrPtr)
  Module._free(tagArrPtr)

  if (resultCode !== 0) {
    throw Error('chacha20poly1305 decryption has failed!')
  }

  return Buffer.from(outputArr)
}

function sha3_256(input) {
  validateBuffer(input)
  const inputLen = input.length
  const inputArrPtr = Module._malloc(inputLen)
  const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)

  const outputLen = 32
  const outputArrPtr = Module._malloc(outputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, outputLen)

  inputArr.set(input)

  Module._emscripten_sha3_256(inputArrPtr, inputLen, outputArrPtr)

  Module._free(inputArrPtr)
  Module._free(outputArrPtr)

  return Buffer.from(outputArr)
}

// used for encoding/decoding seeds to JSON in Daedalus
function cardanoMemoryCombine(input, password) {
  validateBuffer(input)
  validateString(password)

  if (password === '') {
    return input
  }

  const transformedPassword = blake2b(Buffer.from(password, 'utf-8'), 32)
  const transformedPasswordLen = transformedPassword.length
  const transformedPasswordArrPtr = Module._malloc(transformedPasswordLen)
  const transformedPasswordArr = new Uint8Array(Module.HEAPU8.buffer, transformedPasswordArrPtr, transformedPasswordLen)

  const inputLen = input.length
  const inputArrPtr = Module._malloc(inputLen)
  const inputArr = new Uint8Array(Module.HEAPU8.buffer, inputArrPtr, inputLen)

  const outputArrPtr = Module._malloc(inputLen)
  const outputArr = new Uint8Array(Module.HEAPU8.buffer, outputArrPtr, inputLen)

  inputArr.set(input)
  transformedPasswordArr.set(transformedPassword)

  Module._emscripten_cardano_memory_combine(transformedPasswordArrPtr, transformedPasswordLen, inputArrPtr, outputArrPtr, inputLen)

  Module._free(inputArrPtr)
  Module._free(outputArrPtr)
  Module._free(transformedPasswordArrPtr)

  return Buffer.from(outputArr)
}

module.exports = {
  blake2b,
  chacha20poly1305Decrypt,
  chacha20poly1305Encrypt,
  hmac_sha512,
  sha3_256,
  cardanoMemoryCombine,
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"../utils/validation":33,"buffer":37}],2:[function(require,module,exports){
(function (Buffer){
const signing = require("./signing")
const derivation = require("./key-derivation")
const Module = require('../lib.js')

function kadenaMnemonicToRootKeypair(pwd, mnemonic) {
  const pwdBuf = Buffer.from(pwd)
  return derivation.mnemonicToRootKeypairV3(mnemonic, pwdBuf)
}

function kadenaChangePassword(key, oldPwd, newPwd) {
  const keyBuf = Buffer.from(key)
  const oldPwdBuf = Buffer.from(oldPwd)
  const newPwdBuf = Buffer.from(newPwd)
  const newPrv = derivation.changePassword(keyBuf, oldPwdBuf, newPwdBuf)
  return newPrv.buffer;
}

function kadenaGenMnemonic() {
  return derivation.genMnemonic();
}

function kadenaCheckMnemonic(mnem) {
  return derivation.checkMnemonic(mnem);
}

function kadenaGenKeypair(pwd, root, index) {
  const derivationScheme = 2;
  const rootBuffer = Buffer.from(root)
  const pwdBuf = Buffer.from(pwd)
  const xprv = derivation.derivePrivate(pwdBuf, rootBuffer, index, derivationScheme);
  const xpub = new Buffer(xprv.slice(64, 96))
  return [xprv.buffer, xpub.buffer];
}

function kadenaSign(pwd, msg, xprv) {
  const xprvBuf = Buffer.from(xprv);
  const msgBuf = Buffer.from(msg)
  const pwdBuf = Buffer.from(pwd)
  return signing.sign(msgBuf, xprvBuf, pwdBuf).buffer;
}

function kadenaGetPublic(prvKey) {
  const prvBuffer = Buffer.from(prvKey)
  const xpub = new Buffer(prvBuffer.slice(64, 96))
  return xpub.buffer;
}
 
function kadenaVerify(msg, publicKey, sig) {
  const msgBuf = Buffer.from(msg);
  const pubKeyBuf = Buffer.from(publicKey);
  const sigBuf = Buffer.from(sig);
  return signing.verify(msgBuf, pubKeyBuf, sigBuf);
}

module.exports = {
  kadenaGenMnemonic,
  kadenaCheckMnemonic,
  kadenaMnemonicToRootKeypair,
  kadenaGenKeypair,
  kadenaGetPublic,
  kadenaSign,
  kadenaVerify,
  kadenaChangePassword
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"./key-derivation":3,"./signing":4,"buffer":37}],3:[function(require,module,exports){
(function (Buffer){
const bip39 = require('bip39')

const {validateBuffer, validateDerivationIndex, validateDerivationScheme, validateMnemonic} = require("../utils/validation")
const crypto = require("./crypto-primitives")
const pbkdf2 = require('../utils/pbkdf2')
const Module = require('../lib.js')

function genMnemonic() {
  return bip39.generateMnemonic()
}

function checkMnemonic(mnem) {
  return bip39.validateMnemonic(mnem);
}

async function mnemonicToRootKeypair(mnemonic, derivationScheme) {
  if (derivationScheme === 1) {
    return mnemonicToRootKeypairV1(mnemonic)
  } else if (derivationScheme === 2) {
    return mnemonicToRootKeypairV2(mnemonic, '')
  } else if (derivationScheme === 3) {
    // Note, this is different from the derivation scheme value used by cardano-crypto.hs
    return mnemonicToRootKeypairV3(mnemonic, '')
  } else {
    throw Error(`Derivation scheme ${derivationScheme} not implemented`)
  }
}

function mnemonicToRootKeypairV3(mnemonic, pwd) {
  validateMnemonic(mnemonic)
  const seed = Buffer.from(bip39.mnemonicToSeedSync(mnemonic), 'hex')
  return seedToKeypairV1(pwd, seed)
}

function mnemonicToRootKeypairV1(mnemonic) {
  const seed = mnemonicToSeedV1(mnemonic)
  return seedToKeypairV1(seed)
}

function mnemonicToSeedV1(mnemonic) {
  validateMnemonic(mnemonic)
  const entropy = Buffer.from(bip39.mnemonicToEntropy(mnemonic), 'hex')
  return cborEncodeBuffer(crypto.blake2b(cborEncodeBuffer(entropy), 32))
}

function seedToKeypairV1(pwd, seed) {
  let result
  for (let i = 1; result === undefined && i <= 1000; i++) {
    try {
      const digest = crypto.hmac_sha512(seed, [Buffer.from(`Root Seed Chain ${i}`, 'ascii')])
      const tempSeed = digest.slice(0, 32)
      const chainCode = digest.slice(32, 64)

      result = trySeedChainCodeToKeypairV1(pwd, tempSeed, chainCode)

    } catch (e) {
      if (e.name === 'InvalidKeypair') {
        continue
      }

      throw e
    }
  }

  if (result === undefined) {
    const e = new Error('Secret key generation from mnemonic is looping forever')
    e.name = 'RuntimeException'
    throw e
  }

  return result
}

function trySeedChainCodeToKeypairV1(pwd, seed, chainCode) {
  validateBuffer(seed, 32)
  validateBuffer(chainCode, 32)
  validateBuffer(pwd)

  const pwdLen = pwd.length
  const pwdArrPtr = Module._malloc(pwdLen)
  const pwdArr = new Uint8Array(Module.HEAPU8.buffer, pwdArrPtr, pwdLen)
  const seedArrPtr = Module._malloc(32)
  const seedArr = new Uint8Array(Module.HEAPU8.buffer, seedArrPtr, 32)
  const chainCodeArrPtr = Module._malloc(32)
  const chainCodeArr = new Uint8Array(Module.HEAPU8.buffer, chainCodeArrPtr, 32)
  const keypairArrPtr = Module._malloc(128)
  const keypairArr = new Uint8Array(Module.HEAPU8.buffer, keypairArrPtr, 128)

  pwdArr.set(pwd)
  seedArr.set(seed)
  chainCodeArr.set(chainCode)

  const returnCode = Module._emscripten_wallet_secret_from_seed(pwdArrPtr, pwdLen, seedArrPtr, chainCodeArrPtr, keypairArrPtr)

  Module._free(pwdArrPtr)
  Module._free(seedArrPtr)
  Module._free(chainCodeArrPtr)
  Module._free(keypairArrPtr)

  if (returnCode === 1) {
    const e = new Error('Invalid keypair')
    e.name = 'InvalidKeypair'

    throw e
  }

  return Buffer.from(keypairArr)
}

async function mnemonicToRootKeypairV2(mnemonic, password) {
  const seed = mnemonicToSeedV2(mnemonic)
  const rootSecret = await seedToKeypairV2(seed, password)

  return seedToKeypairV2(seed, password)
}

function mnemonicToSeedV2(mnemonic) {
  validateMnemonic(mnemonic)
  let entropy = Buffer.from(bip39.mnemonicToEntropy(mnemonic), 'hex')
  return entropy;
}

async function seedToKeypairV2(seed, password) {
  const xprv = await pbkdf2(password, seed, 4096, 96, 'sha512')

  xprv[0] &= 248
  xprv[31] &= 31
  xprv[31] |= 64

  const publicKey = toPublic(xprv.slice(0, 64))

  return Buffer.concat([xprv.slice(0, 64), publicKey, xprv.slice(64,)])
}

function toPublic(privateKey) {
  validateBuffer(privateKey, 64)

  const privateKeyArrPtr = Module._malloc(64)
  const privateKeyArr = new Uint8Array(Module.HEAPU8.buffer, privateKeyArrPtr, 64)
  const publicKeyArrPtr = Module._malloc(32)
  const publicKeyArr = new Uint8Array(Module.HEAPU8.buffer, publicKeyArrPtr, 32)

  privateKeyArr.set(privateKey)

  Module._emscripten_to_public(privateKeyArrPtr, publicKeyArrPtr)

  Module._free(privateKeyArrPtr)
  Module._free(publicKeyArrPtr)

  return Buffer.from(publicKeyArr)
}

function derivePrivate(pwd, parentKey, index, derivationScheme) {
  validateBuffer(parentKey, 128)
  validateDerivationIndex(index)
  validateDerivationScheme(derivationScheme)
  validateBuffer(pwd)

  const pwdLen = pwd.length
  const pwdArrPtr = Module._malloc(pwdLen)
  const pwdArr = new Uint8Array(Module.HEAPU8.buffer, pwdArrPtr, pwdLen)
  const parentKeyArrPtr = Module._malloc(128)
  const parentKeyArr = new Uint8Array(Module.HEAPU8.buffer, parentKeyArrPtr, 128)
  const childKeyArrPtr = Module._malloc(128)
  const childKeyArr = new Uint8Array(Module.HEAPU8.buffer, childKeyArrPtr, 128)

  pwdArr.set(pwd)
  parentKeyArr.set(parentKey)

  Module._emscripten_derive_private(pwdArrPtr, pwdLen, parentKeyArrPtr, index, childKeyArrPtr, derivationScheme)
  Module._free(parentKeyArrPtr)
  Module._free(childKeyArrPtr)

  return Buffer.from(childKeyArr)
}

function changePassword(inputKey, oldPwd, newPwd) {
  validateBuffer(inputKey, 128)
  validateBuffer(oldPwd)
  validateBuffer(newPwd)

  const oldPwdLen = oldPwd.length
  const oldPwdArrPtr = Module._malloc(oldPwdLen)
  const oldPwdArr = new Uint8Array(Module.HEAPU8.buffer, oldPwdArrPtr, oldPwdLen)

  const newPwdLen = newPwd.length
  const newPwdArrPtr = Module._malloc(newPwdLen)
  const newPwdArr = new Uint8Array(Module.HEAPU8.buffer, newPwdArrPtr, newPwdLen)

  const inputKeyArrPtr = Module._malloc(128)
  const inputKeyArr = new Uint8Array(Module.HEAPU8.buffer, inputKeyArrPtr, 128)

  const newKeyArrPtr = Module._malloc(128)
  const newKeyArr = new Uint8Array(Module.HEAPU8.buffer, newKeyArrPtr, 128)

  oldPwdArr.set(oldPwd)
  newPwdArr.set(newPwd)
  inputKeyArr.set(inputKey)

  Module._emscripten_wallet_change_pass(inputKeyArrPtr, oldPwdArrPtr, oldPwdLen, newPwdArrPtr, newPwdLen, newKeyArrPtr)
  Module._free(oldPwdArrPtr)
  Module._free(newPwdArrPtr)
  Module._free(inputKeyArrPtr)
  Module._free(newKeyArrPtr)

  return Buffer.from(newKeyArr)
}

function derivePublic(parentExtPubKey, index, derivationScheme) {
  validateBuffer(parentExtPubKey, 64)
  validateDerivationIndex(index)
  validateDerivationScheme(derivationScheme)

  const parentPubKey = parentExtPubKey.slice(0, 32)
  const parentChainCode = parentExtPubKey.slice(32, 64)

  const parentPubKeyArrPtr = Module._malloc(32)
  const parentPubKeyArr = new Uint8Array(Module.HEAPU8.buffer, parentPubKeyArrPtr, 32)
  const parentChainCodeArrPtr = Module._malloc(32)
  const parentChainCodeArr = new Uint8Array(Module.HEAPU8.buffer, parentChainCodeArrPtr, 32)

  const childPubKeyArrPtr = Module._malloc(32)
  const childPubKeyArr = new Uint8Array(Module.HEAPU8.buffer, childPubKeyArrPtr, 32)
  const childChainCodeArrPtr = Module._malloc(32)
  const childChainCodeArr = new Uint8Array(Module.HEAPU8.buffer, childChainCodeArrPtr, 32)

  parentPubKeyArr.set(parentPubKey)
  parentChainCodeArr.set(parentChainCode)

  const resultCode = Module._emscripten_derive_public(parentPubKeyArrPtr, parentChainCodeArrPtr, index, childPubKeyArrPtr, childChainCodeArrPtr, derivationScheme)

  Module._free(parentPubKeyArrPtr)
  Module._free(parentChainCodeArrPtr)
  Module._free(parentPubKeyArrPtr)
  Module._free(parentChainCodeArrPtr)

  if (resultCode !== 0) {
    throw Error(`derivePublic has exited with code ${resultCode}`)
  }

  return Buffer.concat([Buffer.from(childPubKeyArr), Buffer.from(childChainCodeArr)])
}

function cborEncodeBuffer(input) {
  validateBuffer(input)

  const len = input.length
  let cborPrefix = []

  if (len < 24) {
    cborPrefix = [0x40 + len]
  } else if (len < 256) {
    cborPrefix = [0x58, len]
  } else {
    throw Error('CBOR encode for more than 256 bytes not yet implemented')
  }

  return Buffer.concat([Buffer.from(cborPrefix), input])
}

module.exports = {
  mnemonicToRootKeypair,
  mnemonicToRootKeypairV3,
  derivePublic,
  derivePrivate,
  toPublic,
  changePassword,
  genMnemonic,
  checkMnemonic,
  _mnemonicToSeedV1: mnemonicToSeedV1,
  _seedToKeypairV1: seedToKeypairV1,
  _seedToKeypairV2: seedToKeypairV2,
  _mnemonicToSeedV2: mnemonicToSeedV2,
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"../utils/pbkdf2":32,"../utils/validation":33,"./crypto-primitives":1,"bip39":8,"buffer":37}],4:[function(require,module,exports){
(function (Buffer){

const {validateBuffer} = require("../utils/validation")
const Module = require('../lib.js')

function sign(msg, keypair, pwd) {
  validateBuffer(msg)
  validateBuffer(pwd)
  validateBuffer(keypair, 128)

  const msgLen = msg.length
  const msgArrPtr = Module._malloc(msgLen)
  const msgArr = new Uint8Array(Module.HEAPU8.buffer, msgArrPtr, msgLen)
  const keypairArrPtr = Module._malloc(128)
  const keypairArr = new Uint8Array(Module.HEAPU8.buffer, keypairArrPtr, 128)
  const sigPtr = Module._malloc(64)
  const sigArr = new Uint8Array(Module.HEAPU8.buffer, sigPtr, 64)
  const pwdLen = pwd.length
  const pwdArrPtr = Module._malloc(pwdLen)
  const pwdArr = new Uint8Array(Module.HEAPU8.buffer, pwdArrPtr, pwdLen)

  msgArr.set(msg)
  keypairArr.set(keypair)
  pwdArr.set(pwd)

  Module._emscripten_sign(pwdArrPtr, pwdLen, keypairArrPtr, msgArrPtr, msgLen, sigPtr)
  Module._free(msgArrPtr)
  Module._free(keypairArrPtr)
  Module._free(sigPtr)
  Module._free(pwdArrPtr)

  return Buffer.from(sigArr)
}

function verify(msg, publicKey, sig) {
  validateBuffer(msg)
  validateBuffer(publicKey, 32)
  validateBuffer(sig, 64)

  const msgLen = msg.length
  const msgArrPtr = Module._malloc(msgLen)
  const msgArr = new Uint8Array(Module.HEAPU8.buffer, msgArrPtr, msgLen)
  const publicKeyArrPtr = Module._malloc(32)
  const publicKeyArr = new Uint8Array(Module.HEAPU8.buffer, publicKeyArrPtr, 32)
  const sigPtr = Module._malloc(64)
  const sigArr = new Uint8Array(Module.HEAPU8.buffer, sigPtr, 64)

  msgArr.set(msg)
  publicKeyArr.set(publicKey)
  sigArr.set(sig)

  const result = Module._emscripten_verify(msgArrPtr, msgLen, publicKeyArrPtr, sigPtr) === 0

  Module._free(msgArrPtr)
  Module._free(publicKeyArrPtr)
  Module._free(sigPtr)

  return result
}

module.exports = {
  sign,
  verify,
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"../utils/validation":33,"buffer":37}],5:[function(require,module,exports){
const kadena = require('./features/kadena-features')
const Module = require('./lib.js')

module.exports = {
  ...kadena,
}

},{"./features/kadena-features":2,"./lib.js":6}],6:[function(require,module,exports){
(function (process,Buffer,__dirname){
var Module=typeof Module!=="undefined"?Module:{};var moduleOverrides={};var key;for(key in Module){if(Module.hasOwnProperty(key)){moduleOverrides[key]=Module[key]}}var arguments_=[];var thisProgram="./this.program";var quit_=function(status,toThrow){throw toThrow};var ENVIRONMENT_IS_WEB=false;var ENVIRONMENT_IS_WORKER=false;var ENVIRONMENT_IS_NODE=false;var ENVIRONMENT_IS_SHELL=false;ENVIRONMENT_IS_WEB=typeof window==="object";ENVIRONMENT_IS_WORKER=typeof importScripts==="function";ENVIRONMENT_IS_NODE=typeof process==="object"&&typeof process.versions==="object"&&typeof process.versions.node==="string";ENVIRONMENT_IS_SHELL=!ENVIRONMENT_IS_WEB&&!ENVIRONMENT_IS_NODE&&!ENVIRONMENT_IS_WORKER;var scriptDirectory="";function locateFile(path){if(Module["locateFile"]){return Module["locateFile"](path,scriptDirectory)}return scriptDirectory+path}var read_,readAsync,readBinary,setWindowTitle;var nodeFS;var nodePath;if(ENVIRONMENT_IS_NODE){if(ENVIRONMENT_IS_WORKER){scriptDirectory=require("path").dirname(scriptDirectory)+"/"}else{scriptDirectory=__dirname+"/"}read_=function shell_read(filename,binary){var ret=tryParseAsDataURI(filename);if(ret){return binary?ret:ret.toString()}if(!nodeFS)nodeFS=require("fs");if(!nodePath)nodePath=require("path");filename=nodePath["normalize"](filename);return nodeFS["readFileSync"](filename,binary?null:"utf8")};readBinary=function readBinary(filename){var ret=read_(filename,true);if(!ret.buffer){ret=new Uint8Array(ret)}assert(ret.buffer);return ret};if(process["argv"].length>1){thisProgram=process["argv"][1].replace(/\\/g,"/")}arguments_=process["argv"].slice(2);if(typeof module!=="undefined"){module["exports"]=Module}process["on"]("unhandledRejection",abort);quit_=function(status){process["exit"](status)};Module["inspect"]=function(){return"[Emscripten Module object]"}}else if(ENVIRONMENT_IS_SHELL){if(typeof read!="undefined"){read_=function shell_read(f){var data=tryParseAsDataURI(f);if(data){return intArrayToString(data)}return read(f)}}readBinary=function readBinary(f){var data;data=tryParseAsDataURI(f);if(data){return data}if(typeof readbuffer==="function"){return new Uint8Array(readbuffer(f))}data=read(f,"binary");assert(typeof data==="object");return data};if(typeof scriptArgs!="undefined"){arguments_=scriptArgs}else if(typeof arguments!="undefined"){arguments_=arguments}if(typeof quit==="function"){quit_=function(status){quit(status)}}if(typeof print!=="undefined"){if(typeof console==="undefined")console={};console.log=print;console.warn=console.error=typeof printErr!=="undefined"?printErr:print}}else if(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER){if(ENVIRONMENT_IS_WORKER){scriptDirectory=self.location.href}else if(document.currentScript){scriptDirectory=document.currentScript.src}if(scriptDirectory.indexOf("blob:")!==0){scriptDirectory=scriptDirectory.substr(0,scriptDirectory.lastIndexOf("/")+1)}else{scriptDirectory=""}{read_=function shell_read(url){try{var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.send(null);return xhr.responseText}catch(err){var data=tryParseAsDataURI(url);if(data){return intArrayToString(data)}throw err}};if(ENVIRONMENT_IS_WORKER){readBinary=function readBinary(url){try{var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.responseType="arraybuffer";xhr.send(null);return new Uint8Array(xhr.response)}catch(err){var data=tryParseAsDataURI(url);if(data){return data}throw err}}}readAsync=function readAsync(url,onload,onerror){var xhr=new XMLHttpRequest;xhr.open("GET",url,true);xhr.responseType="arraybuffer";xhr.onload=function xhr_onload(){if(xhr.status==200||xhr.status==0&&xhr.response){onload(xhr.response);return}var data=tryParseAsDataURI(url);if(data){onload(data.buffer);return}onerror()};xhr.onerror=onerror;xhr.send(null)}}setWindowTitle=function(title){document.title=title}}else{}var out=Module["print"]||console.log.bind(console);var err=Module["printErr"]||console.warn.bind(console);for(key in moduleOverrides){if(moduleOverrides.hasOwnProperty(key)){Module[key]=moduleOverrides[key]}}moduleOverrides=null;if(Module["arguments"])arguments_=Module["arguments"];if(Module["thisProgram"])thisProgram=Module["thisProgram"];if(Module["quit"])quit_=Module["quit"];var wasmBinary;if(Module["wasmBinary"])wasmBinary=Module["wasmBinary"];var noExitRuntime;if(Module["noExitRuntime"])noExitRuntime=Module["noExitRuntime"];var WebAssembly={Memory:function(opts){this.buffer=new ArrayBuffer(opts["initial"]*65536);this.grow=function(amount){var ret=__growWasmMemory(amount);return ret}},Table:function(opts){var ret=new Array(opts["initial"]);ret.set=function(i,func){ret[i]=func};ret.get=function(i){return ret[i]};return ret},Module:function(binary){},Instance:function(module,info){this.exports=(
// EMSCRIPTEN_START_ASM
function a(asmLibraryArg,wasmMemory,wasmTable){function b(global,env,buffer){var memory=env.memory;var c=wasmTable;var d=new global.Int8Array(buffer);var e=new global.Int16Array(buffer);var f=new global.Int32Array(buffer);var g=new global.Uint8Array(buffer);var h=new global.Uint16Array(buffer);var i=new global.Uint32Array(buffer);var j=new global.Float32Array(buffer);var k=new global.Float64Array(buffer);var l=global.Math.imul;var m=global.Math.fround;var n=global.Math.abs;var o=global.Math.clz32;var p=global.Math.min;var q=global.Math.max;var r=global.Math.floor;var s=global.Math.ceil;var t=global.Math.sqrt;var u=env.abort;var v=global.NaN;var w=global.Infinity;var x=env.a;var y=env.b;var z=env.c;var A=5274608;var B=0;
// EMSCRIPTEN_START_FUNCS
function fa(a,b){var c=0,d=0,e=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0,ea=0,fa=0,ga=0,ha=0,ia=0,ja=0,ka=0,la=0,ma=0,na=0,oa=0,pa=0,qa=0,ra=0,sa=0,ta=0,ua=0;c=A-256|0;A=c;while(1){Y=R<<3;ga=Y+(c+128|0)|0;Y=b+Y|0;ba=g[Y+4|0]|g[Y+5|0]<<8|(g[Y+6|0]<<16|g[Y+7|0]<<24);f[ga>>2]=g[Y|0]|g[Y+1|0]<<8|(g[Y+2|0]<<16|g[Y+3|0]<<24);f[ga+4>>2]=ba;R=R+1|0;if((R|0)!=16){continue}break}b=f[a+60>>2];f[c+56>>2]=f[a+56>>2];f[c+60>>2]=b;b=f[a+52>>2];f[c+48>>2]=f[a+48>>2];f[c+52>>2]=b;b=f[a+44>>2];f[c+40>>2]=f[a+40>>2];f[c+44>>2]=b;b=f[a+36>>2];f[c+32>>2]=f[a+32>>2];f[c+36>>2]=b;b=f[a+28>>2];f[c+24>>2]=f[a+24>>2];f[c+28>>2]=b;b=f[a+20>>2];f[c+16>>2]=f[a+16>>2];f[c+20>>2]=b;b=f[a+4>>2];f[c>>2]=f[a>>2];f[c+4>>2]=b;b=f[a+12>>2];f[c+8>>2]=f[a+8>>2];f[c+12>>2]=b;f[c+88>>2]=1595750129;f[c+92>>2]=-1521486534;f[c+80>>2]=-23791573;f[c+84>>2]=1013904242;F=f[a+72>>2];ha=f[a+76>>2];la=f[a+80>>2];ea=f[a+84>>2];T=f[a+64>>2];aa=f[a+68>>2];b=f[a+92>>2]^1541459225;k=b;o=f[a+88>>2]^327033209;f[c+120>>2]=o;f[c+124>>2]=b;Y=c;ga=f[c+136>>2];Z=ga;ja=f[c+140>>2];ba=ja;X=f[c+132>>2];ka=f[c+36>>2];b=ka+f[c+4>>2]|0;$=f[c+32>>2];R=f[c>>2];J=$+R|0;if(J>>>0<R>>>0){b=b+1|0}R=J+f[c+128>>2]|0;b=b+X|0;b=R>>>0<J>>>0?b+1|0:b;d=ba;ba=b;b=d+b|0;X=R+Z|0;if(X>>>0<R>>>0){b=b+1|0}Z=b;aa=jb(R^T^-1377402159,aa^ba^1359893119,32);b=B;v=b;b=b+1779033703|0;R=aa+ -205731576|0;if(R>>>0<4089235720){b=b+1|0}ba=R;T=b;R=jb($^R,ka^b,40);ka=B;b=ka+Z|0;X=R+X|0;if(X>>>0<R>>>0){b=b+1|0}C=X;f[Y>>2]=C;f[Y+4>>2]=b;J=b;x=jb(C^aa,v^b,48);b=B;I=b;f[c+96>>2]=x;f[c+100>>2]=b;b=b+T|0;Z=x+ba|0;if(Z>>>0<ba>>>0){b=b+1|0}aa=Z;f[c+64>>2]=aa;f[Y+68>>2]=b;H=b;e=jb(R^aa,b^ka,1);b=B;m=b;f[c+32>>2]=e;f[c+36>>2]=b;b=f[c+148>>2];u=b;R=b;ba=f[c+44>>2];b=ba+f[c+12>>2]|0;T=f[c+40>>2];Z=f[c+8>>2];X=T+Z|0;if(X>>>0<Z>>>0){b=b+1|0}v=f[c+144>>2];Z=v+X|0;b=b+R|0;b=Z>>>0<X>>>0?b+1|0:b;X=Z;ka=f[c+152>>2];Z=X+ka|0;R=b;b=b+f[c+156>>2]|0;b=Z>>>0<ka>>>0?b+1|0:b;ka=Z;Z=b;F=jb(F^X^725511199,R^ha^-1694144372,32);b=B;ha=b;d=T;b=b+ -1150833019|0;R=F+ -2067093701|0;if(R>>>0<2227873595){b=b+1|0}T=R;i=ba;ba=b;R=jb(d^R,i^b,40);E=B;b=E+Z|0;X=R+ka|0;if(X>>>0<R>>>0){b=b+1|0}M=X;f[Y+8>>2]=M;f[Y+12>>2]=b;s=b;N=jb(F^M,ha^b,48);b=B;S=b;f[c+104>>2]=N;f[c+108>>2]=b;b=b+ba|0;ba=N+T|0;if(ba>>>0<T>>>0){b=b+1|0}t=ba;j=b;f[Y+72>>2]=t;f[Y+76>>2]=b;ba=f[c+168>>2];F=ba;ka=f[c+172>>2];ha=ka;b=f[c+164>>2];ia=b;Z=b;X=f[c+52>>2];b=X+f[c+20>>2]|0;$=f[c+48>>2];T=f[c+16>>2];K=$+T|0;if(K>>>0<T>>>0){b=b+1|0}z=f[c+160>>2];T=z+K|0;b=b+Z|0;Z=T;T=T>>>0<K>>>0?b+1|0:b;b=T+ha|0;d=F;F=Z;ha=d+F|0;if(ha>>>0<F>>>0){b=b+1|0}F=b;T=jb(Z^la^-79577749,T^ea^528734635,32);b=B;la=b;b=b+1013904242|0;Z=T+ -23791573|0;if(Z>>>0<4271175723){b=b+1|0}q=Z;K=b;ea=jb($^q,X^b,40);U=B;b=U+F|0;Z=ea;F=Z+ha|0;if(F>>>0<Z>>>0){b=b+1|0}X=F;f[Y+16>>2]=F;f[Y+20>>2]=b;w=b;h=jb(T^F,la^b,48);b=B;P=b;f[c+112>>2]=h;f[c+116>>2]=b;T=c;b=f[c+188>>2];ha=b;Y=f[c+184>>2];i=Y;$=b;la=f[c+180>>2];d=la;r=f[c+60>>2];b=r+f[c+28>>2]|0;n=f[c+56>>2];F=f[c+24>>2];l=n+F|0;if(l>>>0<F>>>0){b=b+1|0}Z=f[c+176>>2];F=Z+l|0;b=b+d|0;b=F>>>0<l>>>0?b+1|0:b;d=$;$=b;b=d+b|0;d=i;i=F;d=d+i|0;if(d>>>0<i>>>0){b=b+1|0}i=b;l=jb(o^F,k^$,32);b=B;y=b;b=b+ -1521486534|0;F=l+1595750129|0;if(F>>>0<1595750129){b=b+1|0}O=F;k=n^F;n=b;$=jb(k,r^b,40);V=B;b=V+i|0;F=$;i=F+d|0;if(i>>>0<F>>>0){b=b+1|0}Q=i;d=b;f[T+24>>2]=i;f[T+28>>2]=b;D=f[c+200>>2];r=D;na=f[c+204>>2];G=na;b=f[c+196>>2];k=b;o=f[c+192>>2];i=o;F=b;R=jb(t^R,j^E,1);p=i;b=J;J=B;b=b+J|0;i=R;C=i+C|0;if(C>>>0<i>>>0){b=b+1|0}i=p+C|0;b=b+F|0;F=i;i=i>>>0<C>>>0?b+1|0:b;b=i+G|0;C=F;E=C+r|0;if(E>>>0<C>>>0){b=b+1|0}G=E;C=b;E=R;r=jb(l^Q,d^y,48);ca=B;R=jb(F^r,i^ca,32);b=B;p=b;F=R;l=b;b=K+P|0;i=h+q|0;if(i>>>0<h>>>0){b=b+1|0}q=b;b=b+l|0;K=i+F|0;if(K>>>0<i>>>0){b=b+1|0}l=J;J=b;F=jb(K^E,l^b,40);L=B;b=L+C|0;C=F;E=C+G|0;if(E>>>0<C>>>0){b=b+1|0}f[T>>2]=E;f[T+4>>2]=b;y=b;W=jb(E^R,p^b,48);b=B;fa=b;f[c+120>>2]=W;f[c+124>>2]=b;b=J+b|0;T=K+W|0;if(T>>>0<K>>>0){b=b+1|0}R=c;l=T;f[c+80>>2]=l;f[c+84>>2]=b;G=b;L=jb(l^F,L^b,1);b=B;_=b;f[c+40>>2]=L;f[c+44>>2]=b;b=f[c+220>>2];C=b;K=f[c+216>>2];sa=K;p=b;F=f[c+208>>2];J=f[c+212>>2];ma=J;T=jb(i^ea,q^U,1);U=B;b=U+s|0;i=M+T|0;if(i>>>0<T>>>0){b=b+1|0}ea=i+F|0;b=b+ma|0;b=ea>>>0<i>>>0?b+1|0:b;i=b;b=b+p|0;s=ea;q=s+sa|0;if(q>>>0<s>>>0){b=b+1|0}p=q;s=b;ea=jb(x^ea,i^I,32);b=B;I=b;i=ea;q=b;b=n+ca|0;n=r+O|0;if(n>>>0<r>>>0){b=b+1|0}r=q;q=b;b=r+b|0;r=i;i=n;r=r+i|0;if(r>>>0<i>>>0){b=b+1|0}i=r;r=b;T=jb(T^i,U^b,40);M=B;b=M+s|0;s=T;p=s+p|0;if(p>>>0<s>>>0){b=b+1|0}x=b;ca=jb(ea^p,I^b,48);oa=B;b=r+oa|0;ea=i+ca|0;if(ea>>>0<i>>>0){b=b+1|0}r=ea;f[R+88>>2]=r;f[R+92>>2]=b;I=b;O=jb(r^T,b^M,1);b=B;sa=b;f[c+48>>2]=O;f[c+52>>2]=b;T=c;b=f[c+236>>2];s=b;i=f[c+232>>2];U=i;M=b;R=f[c+224>>2];ea=f[c+228>>2];ma=ea;q=jb(n^$,q^V,1);$=B;b=w+$|0;n=q+X|0;if(n>>>0<X>>>0){b=b+1|0}X=n+R|0;b=b+ma|0;b=X>>>0<n>>>0?b+1|0:b;n=b;b=b+M|0;w=X;M=w+U|0;if(M>>>0<w>>>0){b=b+1|0}U=M;w=b;M=jb(N^X,n^S,32);N=B;b=H+N|0;X=M+aa|0;if(X>>>0<aa>>>0){b=b+1|0}n=$;$=b;aa=jb(X^q,n^b,40);H=B;b=H+w|0;n=aa;w=n+U|0;if(w>>>0<n>>>0){b=b+1|0}q=w;n=M^q;M=b;ma=jb(n,N^b,48);da=B;b=$+da|0;$=X+ma|0;if($>>>0<X>>>0){b=b+1|0}w=$;N=b;V=jb(aa^w,H^b,1);b=B;H=b;f[T+56>>2]=V;f[T+60>>2]=b;n=c;S=F;U=J;$=f[c+244>>2];b=y+$|0;T=f[c+240>>2];aa=E+T|0;if(aa>>>0<E>>>0){b=b+1|0}ra=aa;E=b;aa=f[c+248>>2];qa=aa;X=f[c+252>>2];pa=X;b=d+m|0;y=e+Q|0;if(y>>>0<e>>>0){b=b+1|0}d=T;y=d+y|0;b=b+$|0;b=y>>>0<d>>>0?b+1|0:b;d=y;y=b;b=b+pa|0;qa=d+qa|0;if(qa>>>0<d>>>0){b=b+1|0}Q=b;d=jb(d^h,y^P,32);y=e;P=B;b=j+P|0;e=d+t|0;if(e>>>0<t>>>0){b=b+1|0}t=e;h=b;e=jb(y^e,b^m,40);j=B;b=j+Q|0;y=e+qa|0;if(y>>>0<e>>>0){b=b+1|0}Q=b;P=jb(d^y,P^b,48);qa=B;b=h+qa|0;h=t+P|0;if(h>>>0<t>>>0){b=b+1|0}pa=h;m=b;e=jb(e^h,j^b,1);h=B;b=h+E|0;j=e+ra|0;if(j>>>0<e>>>0){b=b+1|0}t=j;j=b;b=b+U|0;d=t;E=d+S|0;if(E>>>0<d>>>0){b=b+1|0}d=b;j=jb(t^ca,j^oa,32);S=e;U=B;b=N+U|0;e=j+w|0;if(e>>>0<w>>>0){b=b+1|0}w=e;t=b;e=jb(S^e,b^h,40);ra=B;b=ra+d|0;d=e+E|0;if(d>>>0<e>>>0){b=b+1|0}N=d;S=b;d=jb(j^d,U^b,48);b=B;ca=b;f[n+96>>2]=d;f[n+100>>2]=b;b=t+b|0;j=d+w|0;if(j>>>0<d>>>0){b=b+1|0}oa=j;f[c+64>>2]=j;f[c+68>>2]=b;U=b;e=jb(e^j,b^ra,1);b=B;w=b;f[c+32>>2]=e;f[c+36>>2]=b;t=c;j=o;E=k;b=_+ia|0;h=z+L|0;if(h>>>0<z>>>0){b=b+1|0}h=h+p|0;b=b+x|0;b=h>>>0<p>>>0?b+1|0:b;n=E;E=b;b=n+b|0;n=h+j|0;if(n>>>0<h>>>0){b=b+1|0}z=n;j=b;h=jb(h^ma,E^da,32);E=B;b=E+m|0;n=h+pa|0;if(n>>>0<h>>>0){b=b+1|0}ia=n;n=b;m=jb(L^ia,_^b,40);da=B;b=da+j|0;z=m+z|0;if(z>>>0<m>>>0){b=b+1|0}p=z;x=b;E=jb(h^p,E^b,48);b=B;ma=b;f[t+104>>2]=E;f[t+108>>2]=b;h=c;b=n+b|0;t=E;j=t+ia|0;if(j>>>0<t>>>0){b=b+1|0}n=j;t=b;f[h+72>>2]=j;f[h+76>>2]=b;z=aa;L=X;b=na+sa|0;j=D+O|0;if(j>>>0<D>>>0){b=b+1|0}j=j+q|0;b=b+M|0;b=j>>>0<q>>>0?b+1|0:b;D=b;b=b+L|0;q=j+z|0;if(q>>>0<j>>>0){b=b+1|0}L=q;z=b;M=jb(j^P,D^qa,32);P=B;b=G+P|0;j=l+M|0;if(j>>>0<l>>>0){b=b+1|0}D=j;q=b;l=jb(O^j,sa^b,40);na=B;b=na+z|0;j=l;z=j+L|0;if(z>>>0<j>>>0){b=b+1|0}G=z;j=M^z;M=b;_=jb(j,P^b,48);b=B;sa=b;f[h+112>>2]=_;f[h+116>>2]=b;z=c;L=R;j=ea;h=jb(m^n,t^da,1);O=B;b=O+ja|0;P=h+ga|0;if(P>>>0<h>>>0){b=b+1|0}m=N+P|0;b=b+S|0;b=m>>>0<N>>>0?b+1|0:b;N=j;j=b;b=N+b|0;S=m+L|0;if(S>>>0<m>>>0){b=b+1|0}ia=S;N=b;S=h;L=m;m=Z;P=la;b=s+H|0;h=i;da=h+V|0;if(da>>>0<h>>>0){b=b+1|0}h=y+da|0;b=b+Q|0;b=h>>>0<y>>>0?b+1|0:b;y=b;b=b+P|0;Q=h+m|0;if(Q>>>0<h>>>0){b=b+1|0}da=Q;Q=b;P=jb(h^W,y^fa,32);W=B;b=I+W|0;h=r+P|0;if(h>>>0<r>>>0){b=b+1|0}ra=h;y=b;m=jb(h^V,b^H,40);qa=B;b=qa+Q|0;h=m;r=h+da|0;if(r>>>0<h>>>0){b=b+1|0}da=r;I=b;Q=jb(P^r,W^b,48);pa=B;h=jb(Q^L,pa^j,32);b=B;L=b;r=h;H=b;b=q+sa|0;j=D+_|0;if(j>>>0<D>>>0){b=b+1|0}D=b;b=b+H|0;H=j+r|0;if(H>>>0<j>>>0){b=b+1|0}q=b;r=jb(H^S,b^O,40);O=B;b=O+N|0;S=r+ia|0;if(S>>>0<r>>>0){b=b+1|0}N=S;S=b;W=jb(h^N,L^b,48);b=B;fa=b;f[z+120>>2]=W;f[z+124>>2]=b;b=q+b|0;z=H+W|0;if(z>>>0<H>>>0){b=b+1|0}h=c;ia=z;f[c+80>>2]=z;f[c+84>>2]=b;z=b;V=jb(r^ia,O^b,1);b=B;r=b;f[c+40>>2]=V;f[c+44>>2]=b;q=v;L=u;H=f[c+128>>2];b=f[c+132>>2];O=b;P=b;j=jb(j^l,D^na,1);D=B;b=x+D|0;l=j+p|0;if(l>>>0<p>>>0){b=b+1|0}p=l;l=l+H|0;b=b+P|0;b=l>>>0<p>>>0?b+1|0:b;p=b;b=b+L|0;x=l+q|0;if(x>>>0<l>>>0){b=b+1|0}L=x;q=b;x=j;j=jb(d^l,p^ca,32);b=B;P=b;l=j;p=b;b=y+pa|0;d=Q+ra|0;if(d>>>0<Q>>>0){b=b+1|0}y=b;b=b+p|0;p=d+l|0;if(p>>>0<d>>>0){b=b+1|0}l=D;D=b;l=jb(p^x,l^b,40);Q=B;b=Q+q|0;x=l+L|0;if(x>>>0<l>>>0){b=b+1|0}q=x;x=b;L=jb(j^q,P^b,48);na=B;b=D+na|0;j=p+L|0;if(j>>>0<p>>>0){b=b+1|0}P=j;f[h+88>>2]=j;f[h+92>>2]=b;D=b;h=jb(l^j,Q^b,1);b=B;j=b;f[c+48>>2]=h;f[c+52>>2]=b;l=c;p=Y;Q=ha;ca=jb(d^m,y^qa,1);b=B;ra=b;m=b;b=C+M|0;d=G+K|0;if(d>>>0<G>>>0){b=b+1|0}y=d;d=d+ca|0;b=b+m|0;m=d;d=d>>>0<y>>>0?b+1|0:b;b=d+Q|0;G=m+p|0;if(G>>>0<m>>>0){b=b+1|0}p=G;y=b;m=jb(m^E,d^ma,32);M=B;b=M+U|0;d=m;E=d+oa|0;if(E>>>0<d>>>0){b=b+1|0}G=b;d=jb(ca^E,ra^b,40);Q=B;b=Q+y|0;p=d+p|0;if(p>>>0<d>>>0){b=b+1|0}ra=p;y=b;U=jb(m^p,M^b,48);qa=B;b=G+qa|0;m=E+U|0;if(m>>>0<E>>>0){b=b+1|0}E=m;m=d^m;d=b;M=jb(m,Q^b,1);b=B;ca=b;f[l+56>>2]=M;f[l+60>>2]=b;m=c;ma=f[c+152>>2];Q=ma;oa=f[c+156>>2];pa=oa;l=e;G=w;b=I+ka|0;p=ba;I=p+da|0;if(I>>>0<p>>>0){b=b+1|0}p=e+I|0;b=b+w|0;w=p;p=p>>>0<e>>>0?b+1|0:b;e=jb(_^w,sa^p,32);I=B;b=I+t|0;n=e+n|0;if(n>>>0<e>>>0){b=b+1|0}t=n;n=b;G=jb(t^l,b^G,40);_=Q;Q=B;b=p+Q|0;l=w+G|0;if(l>>>0<w>>>0){b=b+1|0}w=l;l=_+l|0;b=b+pa|0;b=l>>>0<w>>>0?b+1|0:b;w=b;p=jb(e^l,I^b,48);da=B;b=n+da|0;e=p+t|0;if(e>>>0<t>>>0){b=b+1|0}pa=e;t=b;G=jb(G^e,Q^b,1);b=B;n=b;e=b;b=C+S|0;I=K+N|0;if(I>>>0<N>>>0){b=b+1|0}N=I;I=I+G|0;b=b+e|0;e=I;I=k;k=e>>>0<N>>>0?b+1|0:b;b=I+k|0;I=e+o|0;if(I>>>0<e>>>0){b=b+1|0}N=I;o=b;I=jb(e^L,k^na,32);k=G;G=B;b=d+G|0;e=E+I|0;if(e>>>0<E>>>0){b=b+1|0}d=e;E=b;k=jb(k^e,b^n,40);n=B;b=n+o|0;e=k;o=e+N|0;if(o>>>0<e>>>0){b=b+1|0}N=o;e=b;S=jb(I^o,G^b,48);b=B;_=b;f[m+96>>2]=S;f[m+100>>2]=b;b=E+b|0;m=d+S|0;if(m>>>0<d>>>0){b=b+1|0}sa=m;f[c+64>>2]=m;f[c+68>>2]=b;G=b;Q=jb(k^m,n^b,1);b=B;d=b;f[c+32>>2]=Q;f[c+36>>2]=b;k=c;b=r+ea|0;o=R;m=o+V|0;if(m>>>0<o>>>0){b=b+1|0}o=m+q|0;b=b+x|0;b=o>>>0<q>>>0?b+1|0:b;m=b;b=b+O|0;n=o+H|0;if(n>>>0<o>>>0){b=b+1|0}H=n;E=b;o=jb(o^U,m^qa,32);q=B;b=q+t|0;m=o+pa|0;if(m>>>0<o>>>0){b=b+1|0}t=m;n=b;m=jb(m^V,b^r,40);x=B;b=x+E|0;r=m+H|0;if(r>>>0<m>>>0){b=b+1|0}H=b;U=jb(o^r,q^b,48);b=B;O=b;f[k+104>>2]=U;f[k+108>>2]=b;b=n+b|0;o=t+U|0;if(o>>>0<t>>>0){b=b+1|0}t=o;E=b;f[k+72>>2]=o;f[k+76>>2]=b;n=v;q=u;b=y+ka|0;o=ba;y=o+ra|0;if(y>>>0<o>>>0){b=b+1|0}y=h+y|0;b=b+j|0;o=y;y=q;q=o>>>0<h>>>0?b+1|0:b;b=y+q|0;y=o+n|0;if(y>>>0<o>>>0){b=b+1|0}n=b;o=jb(o^p,q^da,32);I=h;p=B;b=p+z|0;h=o;z=h+ia|0;if(z>>>0<h>>>0){b=b+1|0}q=b;h=jb(I^z,b^j,40);V=B;b=V+n|0;n=h+y|0;if(n>>>0<h>>>0){b=b+1|0}na=n;y=b;L=jb(o^n,p^b,48);b=B;ia=b;f[k+112>>2]=L;f[k+116>>2]=b;j=c;n=T;o=$;k=e;e=jb(m^t,x^E,1);m=B;b=m+J|0;x=e+F|0;if(x>>>0<e>>>0){b=b+1|0}p=x+N|0;b=b+k|0;k=p;p=o;o=k>>>0<x>>>0?b+1|0:b;b=p+o|0;p=k+n|0;if(p>>>0<k>>>0){b=b+1|0}N=p;p=b;x=e;n=i;I=s;b=X+ca|0;e=aa;da=e+M|0;if(da>>>0<e>>>0){b=b+1|0}e=l+da|0;b=b+w|0;b=e>>>0<l>>>0?b+1|0:b;l=b;b=b+I|0;w=e+n|0;if(w>>>0<e>>>0){b=b+1|0}I=w;w=b;e=jb(e^W,l^fa,32);l=B;b=l+D|0;D=e+P|0;if(D>>>0<e>>>0){b=b+1|0}P=D;D=b;n=jb(M^P,ca^b,40);fa=k;ca=B;b=ca+w|0;k=n;w=k+I|0;if(w>>>0<k>>>0){b=b+1|0}I=w;M=b;w=jb(e^w,l^b,48);W=B;e=jb(fa^w,W^o,32);b=B;fa=b;k=e;l=b;b=q+ia|0;o=z+L|0;if(o>>>0<z>>>0){b=b+1|0}q=x;z=b;b=b+l|0;l=k;k=o;l=l+k|0;if(l>>>0<k>>>0){b=b+1|0}x=l;l=b;k=jb(q^x,b^m,40);da=B;b=da+p|0;q=k+N|0;if(q>>>0<k>>>0){b=b+1|0}ra=q;q=b;m=jb(e^ra,fa^b,48);b=B;fa=b;f[j+120>>2]=m;f[j+124>>2]=b;b=l+b|0;l=m+x|0;if(l>>>0<m>>>0){b=b+1|0}e=c;f[c+80>>2]=l;f[c+84>>2]=b;p=b;k=jb(k^l,da^b,1);b=B;j=b;f[c+40>>2]=k;f[c+44>>2]=b;x=Z;N=la;o=jb(h^o,z^V,1);b=B;z=b;h=b;b=H+oa|0;H=r+ma|0;if(H>>>0<r>>>0){b=b+1|0}r=H+o|0;b=b+h|0;h=r;r=h>>>0<H>>>0?b+1|0:b;b=r+N|0;x=h+x|0;if(x>>>0<h>>>0){b=b+1|0}V=x;H=b;x=o;o=jb(h^S,r^_,32);b=B;S=b;r=o;N=b;b=D+W|0;h=w+P|0;if(h>>>0<w>>>0){b=b+1|0}w=b;b=b+N|0;D=h+r|0;if(D>>>0<h>>>0){b=b+1|0}r=z;z=b;r=jb(D^x,r^b,40);W=B;b=W+H|0;x=r+V|0;if(x>>>0<r>>>0){b=b+1|0}N=b;P=jb(o^x,S^b,48);_=B;b=z+_|0;o=D+P|0;if(o>>>0<D>>>0){b=b+1|0}H=o;f[e+88>>2]=o;f[e+92>>2]=b;S=b;o=jb(r^o,W^b,1);b=B;W=b;f[c+48>>2]=o;f[c+52>>2]=b;r=ga;z=ja;D=jb(h^n,w^ca,1);b=B;V=b;h=b;b=y+ha|0;n=Y;w=n+na|0;if(w>>>0<n>>>0){b=b+1|0}n=w+D|0;b=b+h|0;h=n;n=h>>>0<w>>>0?b+1|0:b;b=n+z|0;w=h+r|0;if(w>>>0<h>>>0){b=b+1|0}y=w;r=b;h=jb(h^U,n^O,32);O=B;b=O+G|0;w=h+sa|0;if(w>>>0<h>>>0){b=b+1|0}z=b;n=jb(D^w,V^b,40);V=B;b=V+r|0;D=n+y|0;if(D>>>0<n>>>0){b=b+1|0}G=D;U=b;ca=jb(h^D,O^b,48);da=B;b=z+da|0;h=w+ca|0;if(h>>>0<w>>>0){b=b+1|0}D=h;y=b;V=jb(n^h,V^b,1);b=B;n=b;f[e+56>>2]=V;f[e+60>>2]=b;r=f[c+160>>2];O=r;w=f[c+164>>2];na=w;b=f[c+204>>2];sa=b;h=b;b=d+M|0;M=I+Q|0;if(M>>>0<I>>>0){b=b+1|0}z=f[c+200>>2];I=z+M|0;b=b+h|0;h=I;I=h>>>0<M>>>0?b+1|0:b;b=I+na|0;O=h+O|0;if(O>>>0<h>>>0){b=b+1|0}M=b;L=jb(h^L,I^ia,32);I=Q;Q=B;b=E+Q|0;h=t+L|0;if(h>>>0<t>>>0){b=b+1|0}t=d;d=b;t=jb(I^h,t^b,40);na=B;b=na+M|0;I=t+O|0;if(I>>>0<t>>>0){b=b+1|0}M=b;ia=jb(L^I,Q^b,48);qa=B;b=d+qa|0;d=h+ia|0;if(d>>>0<h>>>0){b=b+1|0}pa=d;Q=b;E=jb(t^d,na^b,1);b=B;t=b;h=b;b=q+ha|0;d=Y;q=d+ra|0;if(q>>>0<d>>>0){b=b+1|0}d=q+E|0;b=b+h|0;b=d>>>0<q>>>0?b+1|0:b;q=d;h=b;b=sa+b|0;d=z;L=d+q|0;if(L>>>0<d>>>0){b=b+1|0}d=b;q=jb(q^P,h^_,32);P=B;b=y+P|0;h=q+D|0;if(h>>>0<D>>>0){b=b+1|0}y=h^E;E=b;t=jb(y,b^t,40);D=B;b=D+d|0;d=t;y=d+L|0;if(y>>>0<d>>>0){b=b+1|0}L=y;O=b;_=jb(q^y,P^b,48);b=B;na=b;f[e+96>>2]=_;f[e+100>>2]=b;b=E+b|0;d=h+_|0;if(d>>>0<h>>>0){b=b+1|0}E=d;f[c+64>>2]=d;f[e+68>>2]=b;d=t^d;t=b;e=jb(d,D^b,1);b=B;D=b;f[c+32>>2]=e;f[c+36>>2]=b;h=c;q=ga;y=ja;b=N+oa|0;d=x+ma|0;if(d>>>0<x>>>0){b=b+1|0}x=d;d=k;x=x+d|0;b=b+j|0;b=x>>>0<d>>>0?b+1|0:b;d=x;x=y;y=b;b=x+b|0;x=d+q|0;if(x>>>0<d>>>0){b=b+1|0}N=x;x=b;q=j;j=jb(d^ca,y^da,32);P=k;y=B;b=y+Q|0;k=j;d=k+pa|0;if(d>>>0<k>>>0){b=b+1|0}Q=d;k=b;q=jb(P^d,b^q,40);ca=B;b=ca+x|0;d=q;x=d+N|0;if(x>>>0<d>>>0){b=b+1|0}oa=x;x=b;j=jb(j^oa,y^b,48);b=B;y=b;f[h+104>>2]=j;f[h+108>>2]=b;b=k+b|0;k=j;d=k+Q|0;if(d>>>0<k>>>0){b=b+1|0}k=b;f[h+72>>2]=d;f[h+76>>2]=b;N=R;Q=ea;b=s+U|0;i=i+G|0;if(i>>>0<G>>>0){b=b+1|0}s=i;i=o;s=s+i|0;b=b+W|0;b=s>>>0<i>>>0?b+1|0:b;i=s;s=b;b=b+Q|0;N=i+N|0;if(N>>>0<i>>>0){b=b+1|0}U=N;G=b;s=jb(i^ia,s^qa,32);P=B;b=p+P|0;i=l+s|0;if(i>>>0<l>>>0){b=b+1|0}ma=i;N=b;l=jb(o^i,W^b,40);ia=B;b=ia+G|0;i=l;o=i+U|0;if(o>>>0<i>>>0){b=b+1|0}U=o;Q=b;s=jb(s^o,P^b,48);b=B;G=b;f[h+112>>2]=s;f[h+116>>2]=b;p=c;P=Z;h=la;i=jb(d^q,k^ca,1);q=B;b=q+u|0;W=i+v|0;if(W>>>0<i>>>0){b=b+1|0}o=L+W|0;b=b+O|0;b=o>>>0<L>>>0?b+1|0:b;L=h;h=b;b=L+b|0;O=o+P|0;if(O>>>0<o>>>0){b=b+1|0}ca=O;L=b;O=i;P=o;o=T;W=$;b=n+C|0;i=K;da=i+V|0;if(da>>>0<i>>>0){b=b+1|0}i=I+da|0;b=b+M|0;b=i>>>0<I>>>0?b+1|0:b;I=b;b=b+W|0;M=i+o|0;if(M>>>0<i>>>0){b=b+1|0}W=M;M=b;m=jb(i^m,I^fa,32);o=V;V=B;b=S+V|0;i=m+H|0;if(i>>>0<H>>>0){b=b+1|0}fa=i;I=b;o=jb(o^i,b^n,40);H=P;da=B;b=da+M|0;i=o;n=i+W|0;if(n>>>0<i>>>0){b=b+1|0}M=n;S=b;P=jb(m^n,V^b,48);V=B;i=jb(H^P,V^h,32);b=B;W=b;h=i;m=b;b=G+N|0;n=s;H=n+ma|0;if(H>>>0<n>>>0){b=b+1|0}n=H;N=b;b=b+m|0;m=h;h=n;m=m+h|0;if(m>>>0<h>>>0){b=b+1|0}ma=m;H=b;m=jb(m^O,b^q,40);q=B;b=q+L|0;h=m;L=h+ca|0;if(L>>>0<h>>>0){b=b+1|0}O=b;h=jb(i^L,W^b,48);b=B;ca=b;f[p+120>>2]=h;f[p+124>>2]=b;b=H+b|0;p=h+ma|0;if(p>>>0<h>>>0){b=b+1|0}i=c;ma=p;f[c+80>>2]=p;f[c+84>>2]=b;W=b;m=jb(m^p,b^q,1);b=B;H=b;f[c+40>>2]=m;f[c+44>>2]=b;q=F;p=J;N=jb(l^n,N^ia,1);b=B;ia=b;n=b;b=x+ka|0;l=ba;x=l+oa|0;if(x>>>0<l>>>0){b=b+1|0}l=x+N|0;b=b+n|0;n=l;l=l>>>0<x>>>0?b+1|0:b;b=l+p|0;p=n+q|0;if(p>>>0<n>>>0){b=b+1|0}oa=p;p=b;n=jb(n^_,l^na,32);b=B;_=b;q=n;x=b;b=I+V|0;l=P+fa|0;if(l>>>0<P>>>0){b=b+1|0}I=b;b=b+x|0;x=l+q|0;if(x>>>0<l>>>0){b=b+1|0}q=N^x;N=b;q=jb(q,ia^b,40);V=B;b=V+p|0;P=q+oa|0;if(P>>>0<q>>>0){b=b+1|0}ra=P;P=b;oa=jb(n^ra,_^b,48);qa=B;b=N+qa|0;n=x+oa|0;if(n>>>0<x>>>0){b=b+1|0}f[i+88>>2]=n;f[i+92>>2]=b;N=b;_=jb(n^q,V^b,1);b=B;q=b;f[c+48>>2]=_;f[c+52>>2]=b;p=c;i=f[c+128>>2];V=i;x=f[c+132>>2];fa=x;I=jb(l^o,I^da,1);b=B;ia=b;o=b;pa=j;b=w+Q|0;j=r+U|0;if(j>>>0<U>>>0){b=b+1|0}l=j;j=j+I|0;b=b+o|0;b=j>>>0<l>>>0?b+1|0:b;l=b;o=jb(pa^j,b^y,32);Q=B;b=Q+t|0;E=o+E|0;if(E>>>0<o>>>0){b=b+1|0}t=E;E=b;U=jb(I^t,ia^b,40);y=V;V=B;b=l+V|0;l=j+U|0;if(l>>>0<j>>>0){b=b+1|0}j=y+l|0;b=b+fa|0;y=j;I=j>>>0<l>>>0?b+1|0:b;ia=jb(o^j,Q^I,48);da=B;b=E+da|0;o=t+ia|0;if(o>>>0<t>>>0){b=b+1|0}j=o;l=b;fa=jb(U^j,V^b,1);b=B;na=b;f[p+56>>2]=fa;f[p+60>>2]=b;t=c;p=i;U=x;o=f[c+192>>2];pa=o;E=f[c+196>>2];ta=E;Q=e;V=D;ua=s;b=S+X|0;s=M+aa|0;if(s>>>0<M>>>0){b=b+1|0}s=e+s|0;b=b+D|0;b=s>>>0<e>>>0?b+1|0:b;D=b;e=jb(ua^s,b^G,32);S=B;b=S+k|0;d=e+d|0;if(d>>>0<e>>>0){b=b+1|0}k=d;d=b;M=jb(k^Q,b^V,40);V=B;b=D+V|0;D=s+M|0;if(D>>>0<s>>>0){b=b+1|0}s=D+pa|0;b=b+ta|0;b=s>>>0<D>>>0?b+1|0:b;D=s;G=b;Q=jb(e^s,S^b,48);pa=B;b=d+pa|0;e=k+Q|0;if(e>>>0<k>>>0){b=b+1|0}ta=e;s=b;M=jb(M^e,V^b,1);b=B;k=b;e=b;b=O+sa|0;d=z+L|0;if(d>>>0<L>>>0){b=b+1|0}z=d;d=d+M|0;b=b+e|0;e=d;d=e>>>0<z>>>0?b+1|0:b;b=d+U|0;p=e+p|0;if(p>>>0<e>>>0){b=b+1|0}z=b;d=jb(e^oa,d^qa,32);L=M;M=B;b=l+M|0;e=d+j|0;if(e>>>0<j>>>0){b=b+1|0}l=e;j=k;k=b;e=jb(L^e,j^b,40);S=B;b=S+z|0;z=e+p|0;if(z>>>0<e>>>0){b=b+1|0}p=b;j=jb(d^z,M^b,48);b=B;sa=b;f[t+96>>2]=j;f[t+100>>2]=b;b=k+b|0;k=j;d=k+l|0;if(d>>>0<k>>>0){b=b+1|0}f[c+64>>2]=d;f[t+68>>2]=b;M=b;V=jb(e^d,S^b,1);b=B;oa=b;f[c+32>>2]=V;f[c+36>>2]=b;e=c;t=Y;l=ha;b=P+ka|0;k=ba;S=k+ra|0;if(S>>>0<k>>>0){b=b+1|0}k=m;S=k+S|0;b=b+H|0;b=S>>>0<k>>>0?b+1|0:b;k=S;L=l;l=b;b=L+b|0;S=k+t|0;if(S>>>0<k>>>0){b=b+1|0}U=S;S=b;k=jb(k^ia,l^da,32);t=m;L=B;b=L+s|0;m=k+ta|0;if(m>>>0<k>>>0){b=b+1|0}s=b;t=jb(t^m,b^H,40);O=B;b=O+S|0;l=t;H=l+U|0;if(H>>>0<l>>>0){b=b+1|0}S=H;U=b;P=jb(k^H,L^b,48);b=B;ia=b;f[e+104>>2]=P;f[e+108>>2]=b;b=s+b|0;k=m+P|0;if(k>>>0<m>>>0){b=b+1|0}m=k;l=b;f[e+72>>2]=k;f[e+76>>2]=b;b=u+q|0;k=v;s=k+_|0;if(s>>>0<k>>>0){b=b+1|0}k=s+y|0;b=b+I|0;b=k>>>0<y>>>0?b+1|0:b;s=b;b=b+w|0;H=k+r|0;if(H>>>0<k>>>0){b=b+1|0}y=H;H=b;k=jb(k^Q,s^pa,32);I=B;b=I+W|0;r=k+ma|0;if(r>>>0<k>>>0){b=b+1|0}L=r;w=b;r=jb(r^_,b^q,40);W=B;b=W+H|0;s=r;H=s+y|0;if(H>>>0<s>>>0){b=b+1|0}_=H;q=b;s=jb(k^H,I^b,48);b=B;ma=b;f[e+112>>2]=s;f[e+116>>2]=b;H=c;y=ga;I=ja;e=jb(m^t,l^O,1);Q=B;b=Q+$|0;t=e+T|0;if(t>>>0<e>>>0){b=b+1|0}k=t+z|0;b=b+p|0;b=k>>>0<z>>>0?b+1|0:b;t=b;b=b+I|0;y=k+y|0;if(y>>>0<k>>>0){b=b+1|0}O=y;z=b;y=e;p=aa;I=X;b=J+na|0;e=F;da=e+fa|0;if(da>>>0<e>>>0){b=b+1|0}e=D+da|0;b=b+G|0;b=e>>>0<D>>>0?b+1|0:b;D=b;b=b+I|0;p=e+p|0;if(p>>>0<e>>>0){b=b+1|0}I=p;G=b;p=jb(e^h,D^ca,32);ca=B;b=N+ca|0;e=n+p|0;if(e>>>0<n>>>0){b=b+1|0}da=e;D=b;h=jb(fa^e,na^b,40);n=k;ra=B;b=ra+G|0;e=h;k=e+I|0;if(k>>>0<e>>>0){b=b+1|0}G=k;k=p^k;p=b;I=jb(k,ca^b,48);ca=B;e=jb(n^I,ca^t,32);b=B;fa=b;k=e;t=b;b=w+ma|0;n=s;w=n+L|0;if(w>>>0<n>>>0){b=b+1|0}n=w;N=y;y=b;b=b+t|0;t=k;k=n;t=t+k|0;if(t>>>0<k>>>0){b=b+1|0}L=t;w=b;t=jb(N^t,b^Q,40);na=B;b=na+z|0;k=t;z=k+O|0;if(z>>>0<k>>>0){b=b+1|0}N=b;k=jb(e^z,fa^b,48);b=B;fa=b;f[H+120>>2]=k;f[H+124>>2]=b;b=w+b|0;w=k+L|0;if(w>>>0<k>>>0){b=b+1|0}e=c;H=w;f[c+80>>2]=w;f[c+84>>2]=b;Q=b;t=jb(t^w,b^na,1);b=B;w=b;f[c+40>>2]=t;f[c+44>>2]=b;L=R;O=ea;n=jb(n^r,y^W,1);b=B;y=b;r=b;b=C+U|0;U=K+S|0;if(U>>>0<S>>>0){b=b+1|0}S=U+n|0;b=b+r|0;r=S;S=r>>>0<U>>>0?b+1|0:b;b=S+O|0;L=r+L|0;if(L>>>0<r>>>0){b=b+1|0}O=L;U=b;L=n;j=jb(j^r,S^sa,32);b=B;W=b;r=j;S=b;b=D+ca|0;n=I+da|0;if(n>>>0<I>>>0){b=b+1|0}D=b;b=b+S|0;I=n+r|0;if(I>>>0<n>>>0){b=b+1|0}r=y;y=b;r=jb(I^L,r^b,40);L=B;b=L+U|0;U=r+O|0;if(U>>>0<r>>>0){b=b+1|0}S=U;U=b;ca=jb(j^S,W^b,48);da=B;b=y+da|0;j=I+ca|0;if(j>>>0<I>>>0){b=b+1|0}sa=j;f[e+88>>2]=j;f[e+92>>2]=b;y=b;e=jb(r^j,L^b,1);b=B;na=b;f[c+48>>2]=e;f[c+52>>2]=b;j=c;r=o;I=E;D=jb(h^n,D^ra,1);b=B;L=b;h=b;b=q+la|0;n=Z;q=n+_|0;if(q>>>0<n>>>0){b=b+1|0}n=q+D|0;b=b+h|0;h=n;n=h>>>0<q>>>0?b+1|0:b;b=n+I|0;q=h+r|0;if(q>>>0<h>>>0){b=b+1|0}I=q;r=b;q=jb(h^P,n^ia,32);O=B;b=M+O|0;h=d+q|0;if(h>>>0<d>>>0){b=b+1|0}d=h;n=b;h=jb(D^d,L^b,40);M=B;b=M+r|0;D=h+I|0;if(D>>>0<h>>>0){b=b+1|0}ra=D;r=b;ia=jb(q^D,O^b,48);qa=B;b=n+qa|0;n=d+ia|0;if(n>>>0<d>>>0){b=b+1|0}D=b;_=jb(h^n,M^b,1);b=B;d=b;f[j+56>>2]=_;f[j+60>>2]=b;q=R;I=ea;b=f[c+236>>2];M=b;L=f[c+232>>2];pa=L;O=b;b=f[c+156>>2];P=b;h=b;b=p+oa|0;p=G+V|0;if(p>>>0<G>>>0){b=b+1|0}W=f[c+152>>2];G=W+p|0;b=b+h|0;h=G;G=h>>>0<p>>>0?b+1|0:b;b=G+O|0;O=h+pa|0;if(O>>>0<h>>>0){b=b+1|0}pa=O;p=b;O=jb(h^s,G^ma,32);ma=B;b=l+ma|0;h=m+O|0;if(h>>>0<m>>>0){b=b+1|0}m=h;s=b;h=jb(V^h,oa^b,40);V=B;b=V+p|0;G=h+pa|0;if(G>>>0<h>>>0){b=b+1|0}p=b;O=jb(O^G,ma^b,48);ma=B;b=s+ma|0;s=m+O|0;if(s>>>0<m>>>0){b=b+1|0}oa=s;s=b;l=jb(h^oa,V^b,1);b=B;h=b;m=b;b=u+N|0;v=v+z|0;if(v>>>0<z>>>0){b=b+1|0}u=v;v=u+l|0;b=b+m|0;b=v>>>0<u>>>0?b+1|0:b;u=b;b=b+I|0;m=v;z=m+q|0;if(z>>>0<m>>>0){b=b+1|0}q=z;m=b;z=jb(v^ca,u^da,32);N=l;l=B;b=D+l|0;v=n+z|0;if(v>>>0<n>>>0){b=b+1|0}n=v;u=b;v=jb(N^n,b^h,40);I=B;b=I+m|0;h=v;m=h+q|0;if(m>>>0<h>>>0){b=b+1|0}ca=m;h=z^m;z=b;h=jb(h,l^b,48);b=B;N=b;f[j+96>>2]=h;f[j+100>>2]=b;b=u+b|0;j=h+n|0;if(j>>>0<h>>>0){b=b+1|0}V=j;f[c+64>>2]=j;f[c+68>>2]=b;D=b;v=jb(v^j,b^I,1);b=B;j=b;f[c+32>>2]=v;f[c+36>>2]=b;u=c;n=F;l=J;b=U+la|0;m=S+Z|0;if(m>>>0<S>>>0){b=b+1|0}q=m;m=t;q=q+m|0;b=b+w|0;b=q>>>0<m>>>0?b+1|0:b;m=q;q=l;l=b;b=q+b|0;q=m+n|0;if(q>>>0<m>>>0){b=b+1|0}n=b;m=jb(m^ia,l^qa,32);I=B;b=I+s|0;s=m+oa|0;if(s>>>0<m>>>0){b=b+1|0}U=s;l=b;s=jb(s^t,b^w,40);oa=B;b=oa+n|0;n=q+s|0;if(n>>>0<s>>>0){b=b+1|0}ia=n;w=b;t=jb(m^n,I^b,48);b=B;S=b;f[u+104>>2]=t;f[u+108>>2]=b;b=l+b|0;m=t;n=m+U|0;if(n>>>0<m>>>0){b=b+1|0}m=n;n=b;f[u+72>>2]=m;f[u+76>>2]=b;l=K;b=r+x|0;K=i+ra|0;if(K>>>0<i>>>0){b=b+1|0}i=e+K|0;b=b+na|0;K=i;i=i>>>0<e>>>0?b+1|0:b;b=i+C|0;C=K;l=C+l|0;if(l>>>0<C>>>0){b=b+1|0}r=l;l=b;i=jb(K^O,i^ma,32);x=B;b=Q+x|0;K=i+H|0;if(K>>>0<H>>>0){b=b+1|0}U=K;H=b;C=jb(e^K,na^b,40);Q=B;b=Q+l|0;e=r+C|0;if(e>>>0<C>>>0){b=b+1|0}O=e;q=b;e=jb(i^e,x^b,48);b=B;l=b;f[u+112>>2]=e;f[u+116>>2]=b;r=c;b=f[c+160>>2];i=f[c+164>>2];u=jb(m^s,n^oa,1);x=b;s=B;b=s+z|0;z=u+ca|0;if(z>>>0<u>>>0){b=b+1|0}K=x+z|0;b=b+i|0;b=K>>>0<z>>>0?b+1|0:b;i=b;b=b+M|0;z=K;x=z+L|0;if(x>>>0<z>>>0){b=b+1|0}M=x;z=b;x=u;I=s;b=d+E|0;u=o+_|0;if(u>>>0<o>>>0){b=b+1|0}u=u+G|0;b=b+p|0;b=u>>>0<G>>>0?b+1|0:b;o=b;b=b+P|0;E=u+W|0;if(E>>>0<u>>>0){b=b+1|0}s=b;u=jb(k^u,o^fa,32);p=B;b=p+y|0;k=u;o=k+sa|0;if(o>>>0<k>>>0){b=b+1|0}L=o;y=b;o=jb(o^_,b^d,40);P=B;b=P+s|0;k=o+E|0;if(k>>>0<o>>>0){b=b+1|0}W=k;G=b;p=jb(u^k,p^b,48);_=B;u=jb(p^K,_^i,32);b=B;d=b;K=u;i=b;b=l+H|0;s=e+U|0;if(s>>>0<e>>>0){b=b+1|0}H=b;b=b+i|0;i=s+K|0;if(i>>>0<s>>>0){b=b+1|0}E=i;k=b;K=jb(i^x,b^I,40);x=B;b=x+z|0;i=K;z=i+M|0;if(z>>>0<i>>>0){b=b+1|0}fa=z;z=b;i=jb(u^fa,d^b,48);b=B;U=b;f[r+120>>2]=i;f[r+124>>2]=b;b=k+b|0;d=i+E|0;if(d>>>0<i>>>0){b=b+1|0}u=c;f[c+80>>2]=d;f[c+84>>2]=b;k=K;K=b;k=jb(k^d,b^x,1);b=B;E=b;f[c+40>>2]=k;f[c+44>>2]=b;r=ba;x=ka;C=jb(s^C,H^Q,1);b=B;H=b;s=b;b=w+ha|0;w=Y;I=w+ia|0;if(I>>>0<w>>>0){b=b+1|0}w=I+C|0;b=b+s|0;s=w;w=s>>>0<I>>>0?b+1|0:b;b=w+x|0;x=s+r|0;if(x>>>0<s>>>0){b=b+1|0}I=x;r=b;x=C;h=jb(h^s,w^N,32);b=B;M=b;s=h;w=b;b=y+_|0;C=p+L|0;if(C>>>0<p>>>0){b=b+1|0}y=w;w=b;b=y+b|0;y=s;s=C;y=y+s|0;if(y>>>0<s>>>0){b=b+1|0}s=H;H=b;s=jb(y^x,s^b,40);p=B;b=p+r|0;x=s+I|0;if(x>>>0<s>>>0){b=b+1|0}Q=x;r=b;I=jb(h^x,M^b,48);L=B;b=H+L|0;h=y+I|0;if(h>>>0<y>>>0){b=b+1|0}N=h;f[u+88>>2]=h;f[u+92>>2]=b;H=b;u=jb(s^h,b^p,1);b=B;h=b;f[c+48>>2]=u;f[c+52>>2]=b;s=c;y=T;p=$;x=jb(o^C,w^P,1);b=B;M=b;o=b;b=q+X|0;C=aa;w=C+O|0;if(w>>>0<C>>>0){b=b+1|0}C=w+x|0;b=b+o|0;o=C;C=o>>>0<w>>>0?b+1|0:b;b=C+p|0;q=o+y|0;if(q>>>0<o>>>0){b=b+1|0}w=b;o=jb(o^t,C^S,32);y=B;b=y+D|0;C=o+V|0;if(C>>>0<o>>>0){b=b+1|0}D=b;t=jb(x^C,M^b,40);x=B;b=x+w|0;q=q+t|0;if(q>>>0<t>>>0){b=b+1|0}O=q;w=b;p=jb(o^q,y^b,48);P=B;b=D+P|0;o=p+C|0;if(o>>>0<C>>>0){b=b+1|0}C=o;o=t^o;t=b;x=jb(o,x^b,1);b=B;S=b;f[s+56>>2]=x;f[s+60>>2]=b;o=c;s=ba;D=ka;M=f[c+200>>2];V=f[c+204>>2];q=v;y=j;ia=e;b=G+ja|0;e=ga;G=e+W|0;if(G>>>0<e>>>0){b=b+1|0}e=v+G|0;b=b+j|0;b=e>>>0<v>>>0?b+1|0:b;v=e;e=b;l=jb(ia^v,b^l,32);G=q;q=B;b=n+q|0;j=m+l|0;if(j>>>0<m>>>0){b=b+1|0}m=j;j=b;y=jb(G^m,b^y,40);n=M;M=B;b=e+M|0;e=v+y|0;if(e>>>0<v>>>0){b=b+1|0}v=n+e|0;b=b+V|0;n=v;v=l^n;l=n>>>0<e>>>0?b+1|0:b;G=jb(v,q^l,48);q=B;b=j+q|0;v=m+G|0;if(v>>>0<m>>>0){b=b+1|0}W=v;j=y^v;y=b;M=jb(j,M^b,1);b=B;e=b;v=b;b=z+ea|0;m=R;j=m+fa|0;if(j>>>0<m>>>0){b=b+1|0}m=j+M|0;b=b+v|0;v=m;m=m>>>0<j>>>0?b+1|0:b;b=m+D|0;j=v;s=j+s|0;if(s>>>0<j>>>0){b=b+1|0}z=s;j=b;s=jb(v^I,m^L,32);D=B;b=t+D|0;v=s+C|0;if(v>>>0<C>>>0){b=b+1|0}C=v;m=b;v=jb(v^M,b^e,40);b=j;j=B;b=b+j|0;e=v;t=e+z|0;if(t>>>0<e>>>0){b=b+1|0}V=t;z=b;e=jb(s^t,D^b,48);b=B;t=b;f[o+96>>2]=e;f[o+100>>2]=b;b=m+b|0;C=e+C|0;if(C>>>0<e>>>0){b=b+1|0}m=C;f[c+64>>2]=m;f[o+68>>2]=b;D=b;I=jb(m^v,b^j,1);b=B;j=b;f[c+32>>2]=I;f[c+36>>2]=b;v=c;C=aa;s=X;b=r+ja|0;o=ga;r=o+Q|0;if(r>>>0<o>>>0){b=b+1|0}r=k+r|0;b=b+E|0;o=r;r=s;s=o>>>0<k>>>0?b+1|0:b;b=r+s|0;r=o+C|0;if(r>>>0<o>>>0){b=b+1|0}M=r;r=b;o=jb(o^p,s^P,32);Q=k;b=y;y=B;b=b+y|0;k=o;C=k+W|0;if(C>>>0<k>>>0){b=b+1|0}p=C;s=b;C=jb(Q^p,b^E,40);P=B;b=P+r|0;k=C;E=k+M|0;if(E>>>0<k>>>0){b=b+1|0}W=E;E=b;k=jb(o^W,y^b,48);b=B;Q=b;f[v+104>>2]=k;f[v+108>>2]=b;b=s+b|0;s=k+p|0;if(s>>>0<k>>>0){b=b+1|0}o=s;s=b;f[v+72>>2]=o;f[v+76>>2]=b;p=f[c+232>>2];M=f[c+236>>2];r=u;y=h;L=G;b=w+$|0;w=T;G=w+O|0;if(G>>>0<w>>>0){b=b+1|0}w=u+G|0;b=b+h|0;h=w;w=h>>>0<u>>>0?b+1|0:b;u=jb(L^h,w^q,32);G=B;b=G+K|0;d=d+u|0;if(d>>>0<u>>>0){b=b+1|0}q=d^r;r=b;L=jb(q,b^y,40);_=B;b=w+_|0;K=h+L|0;if(K>>>0<h>>>0){b=b+1|0}h=K;K=h+p|0;b=b+M|0;w=K;q=w>>>0<h>>>0?b+1|0:b;M=jb(u^w,G^q,48);b=B;O=b;f[v+112>>2]=M;f[v+116>>2]=b;h=c;y=Y;K=ha;b=f[c+128>>2];G=f[c+132>>2];v=jb(o^C,s^P,1);p=b;C=B;b=C+z|0;u=v;z=u+V|0;if(z>>>0<u>>>0){b=b+1|0}u=p+z|0;b=b+G|0;b=u>>>0<z>>>0?b+1|0:b;p=K;K=b;b=p+b|0;y=u+y|0;if(y>>>0<u>>>0){b=b+1|0}P=y;z=b;y=v;G=C;C=F;p=J;V=f[c+164>>2];b=l+S|0;v=n+x|0;if(v>>>0<n>>>0){b=b+1|0}n=v;v=n+f[c+160>>2]|0;b=b+V|0;b=v>>>0<n>>>0?b+1|0:b;n=b;b=b+p|0;l=v+C|0;if(l>>>0<v>>>0){b=b+1|0}C=b;v=jb(i^v,n^U,32);p=B;b=p+H|0;i=v;n=i+N|0;if(n>>>0<i>>>0){b=b+1|0}N=n;n=b;i=jb(x^N,S^b,40);V=B;b=V+C|0;C=i+l|0;if(C>>>0<i>>>0){b=b+1|0}l=C;H=b;p=jb(v^l,p^b,48);x=B;v=jb(p^u,x^K,32);b=B;S=b;u=v;K=b;b=r+O|0;C=d+M|0;if(C>>>0<d>>>0){b=b+1|0}r=b;b=b+K|0;d=u;u=C;K=d+u|0;if(K>>>0<u>>>0){b=b+1|0}U=K;d=b;u=jb(K^y,b^G,40);fa=B;b=fa+z|0;z=u+P|0;if(z>>>0<u>>>0){b=b+1|0}y=b;K=jb(v^z,S^b,48);b=B;S=b;f[h+120>>2]=K;f[h+124>>2]=b;b=d+b|0;h=K;d=h+U|0;if(d>>>0<h>>>0){b=b+1|0}v=c;U=d;f[c+80>>2]=d;f[c+84>>2]=b;G=b;u=jb(u^d,b^fa,1);b=B;h=b;f[c+40>>2]=u;f[c+44>>2]=b;P=f[c+152>>2];fa=f[c+156>>2];b=n+x|0;d=p+N|0;if(d>>>0<p>>>0){b=b+1|0}n=b;p=d;x=b;r=jb(C^L,r^_,1);b=B;N=b;C=b;L=e;b=E+la|0;e=Z;E=e+W|0;if(E>>>0<e>>>0){b=b+1|0}e=E+r|0;b=b+C|0;C=e;W=t;t=e>>>0<E>>>0?b+1|0:b;e=jb(L^e,W^t,32);L=B;b=L+x|0;p=e+p|0;if(p>>>0<e>>>0){b=b+1|0}E=p;p=r^p;r=b;x=jb(p,N^b,40);N=B;b=t+N|0;t=C+x|0;if(t>>>0<C>>>0){b=b+1|0}C=t;t=t+P|0;b=b+fa|0;P=t;p=t>>>0<C>>>0?b+1|0:b;L=jb(e^t,L^p,48);W=B;b=r+W|0;e=E+L|0;if(e>>>0<E>>>0){b=b+1|0}t=e;f[v+88>>2]=e;f[v+92>>2]=b;E=b;x=jb(e^x,b^N,1);b=B;C=b;f[c+48>>2]=x;f[c+52>>2]=b;b=f[c+200>>2];e=f[c+204>>2];d=jb(d^i,n^V,1);N=b;n=B;b=q+n|0;i=d+w|0;if(i>>>0<w>>>0){b=b+1|0}r=i;i=N+i|0;b=b+e|0;b=i>>>0<r>>>0?b+1|0:b;r=i;w=f[c+144>>2];e=i+w|0;i=b;b=b+f[c+148>>2]|0;b=e>>>0<w>>>0?b+1|0:b;w=e;e=b;r=jb(k^r,i^Q,32);q=B;b=D+q|0;i=m+r|0;if(i>>>0<m>>>0){b=b+1|0}k=i;m=b;i=jb(d^i,n^b,40);d=B;b=d+e|0;e=i;n=e+w|0;if(n>>>0<e>>>0){b=b+1|0}e=r^n;r=b;D=jb(e,q^b,48);V=B;b=m+V|0;e=k+D|0;if(e>>>0<k>>>0){b=b+1|0}k=e;e=i^e;i=b;N=jb(e,d^b,1);b=B;Q=b;f[v+56>>2]=N;f[v+60>>2]=b;e=c;m=f[c+216>>2];w=m;q=f[c+220>>2];d=q;_=f[c+196>>2];b=j+H|0;v=l+I|0;if(v>>>0<l>>>0){b=b+1|0}l=v;v=l+f[c+192>>2]|0;b=b+_|0;b=v>>>0<l>>>0?b+1|0:b;l=d;d=b;b=l+b|0;l=v;H=l+w|0;if(H>>>0<l>>>0){b=b+1|0}l=b;d=jb(v^M,d^O,32);w=B;b=s+w|0;v=d+o|0;if(v>>>0<o>>>0){b=b+1|0}o=v;v=j;j=b;v=jb(o^I,v^b,40);I=B;b=I+l|0;l=v+H|0;if(l>>>0<v>>>0){b=b+1|0}H=b;M=jb(d^l,w^b,48);O=B;b=j+O|0;j=o+M|0;if(j>>>0<o>>>0){b=b+1|0}_=j;o=b;s=jb(v^j,I^b,1);b=B;v=b;j=b;b=y+f[c+236>>2]|0;d=z+f[c+232>>2]|0;if(d>>>0<z>>>0){b=b+1|0}w=d;d=d+s|0;b=b+j|0;b=d>>>0<w>>>0?b+1|0:b;j=b;b=q+b|0;w=d+m|0;if(w>>>0<m>>>0){b=b+1|0}m=b;d=jb(d^L,j^W,32);q=s;s=B;b=i+s|0;i=d+k|0;if(i>>>0<k>>>0){b=b+1|0}z=i;j=b;k=jb(q^i,b^v,40);q=B;b=q+m|0;i=k+w|0;if(i>>>0<k>>>0){b=b+1|0}y=i;v=b;i=jb(d^i,s^b,48);b=B;m=b;f[e+96>>2]=i;f[e+100>>2]=b;b=b+j|0;s=i+z|0;if(s>>>0<i>>>0){b=b+1|0}j=s;f[c+64>>2]=j;f[e+68>>2]=b;w=b;I=jb(k^j,b^q,1);b=B;s=b;f[c+32>>2]=I;f[c+36>>2]=b;k=c;d=T;z=$;b=p+ha|0;e=Y;q=e+P|0;if(q>>>0<e>>>0){b=b+1|0}e=u;q=e+q|0;b=b+h|0;b=q>>>0<e>>>0?b+1|0:b;e=q;q=z;z=b;b=q+b|0;q=e+d|0;if(q>>>0<e>>>0){b=b+1|0}d=b;e=jb(e^D,z^V,32);D=B;b=D+o|0;o=e+_|0;if(o>>>0<e>>>0){b=b+1|0}p=o;z=h;h=b;u=jb(o^u,z^b,40);P=B;b=P+d|0;o=u;d=o+q|0;if(d>>>0<o>>>0){b=b+1|0}W=d;z=b;e=jb(e^d,D^b,48);b=B;L=b;f[k+104>>2]=e;f[k+108>>2]=b;o=c;b=h+b|0;h=e+p|0;if(h>>>0<e>>>0){b=b+1|0}k=h;h=b;f[o+72>>2]=k;f[o+76>>2]=b;d=c;D=ga;q=ja;b=C+ea|0;o=R;p=o+x|0;if(p>>>0<o>>>0){b=b+1|0}o=n+p|0;b=b+r|0;b=o>>>0<n>>>0?b+1|0:b;n=b;b=b+q|0;D=o+D|0;if(D>>>0<o>>>0){b=b+1|0}r=b;o=jb(o^M,n^O,32);q=B;b=q+G|0;G=o+U|0;if(G>>>0<o>>>0){b=b+1|0}M=G;n=b;C=jb(G^x,b^C,40);O=B;b=O+r|0;r=C;D=r+D|0;if(D>>>0<r>>>0){b=b+1|0}r=D;D=b;o=jb(o^r,q^b,48);b=B;U=b;f[d+112>>2]=o;f[d+116>>2]=b;q=v;v=jb(k^u,h^P,1);p=y;y=B;b=y+ka|0;u=v;G=u+ba|0;if(G>>>0<u>>>0){b=b+1|0}u=p+G|0;b=b+q|0;b=u>>>0<G>>>0?b+1|0:b;q=u;p=f[c+128>>2];G=u+p|0;u=b;b=b+f[c+132>>2]|0;P=G;G=G>>>0<p>>>0?b+1|0:b;p=v;x=f[c+156>>2];b=H+Q|0;v=l+N|0;if(v>>>0<l>>>0){b=b+1|0}l=v;v=l+f[c+152>>2]|0;b=b+x|0;b=v>>>0<l>>>0?b+1|0:b;H=v;x=f[c+200>>2];l=v+x|0;v=b;b=b+f[c+204>>2]|0;b=l>>>0<x>>>0?b+1|0:b;x=l;l=b;H=jb(H^K,v^S,32);S=B;b=E+S|0;v=t+H|0;if(v>>>0<t>>>0){b=b+1|0}V=v;E=b;K=jb(N^v,Q^b,40);N=q;_=B;b=_+l|0;v=K;t=v+x|0;if(t>>>0<v>>>0){b=b+1|0}l=t;v=H^l;H=b;q=jb(v,S^b,48);S=B;v=jb(N^q,S^u,32);b=B;N=b;u=v;x=b;b=n+U|0;n=o+M|0;if(n>>>0<o>>>0){b=b+1|0}t=n;M=p;n=b;b=b+x|0;p=u;u=t;p=p+u|0;if(p>>>0<u>>>0){b=b+1|0}u=y;y=b;u=jb(M^p,u^b,40);x=B;b=x+G|0;M=u+P|0;if(M>>>0<u>>>0){b=b+1|0}P=M;G=b;M=jb(v^M,N^b,48);b=B;N=b;f[d+120>>2]=M;f[d+124>>2]=b;b=y+b|0;d=p+M|0;if(d>>>0<p>>>0){b=b+1|0}Q=d;f[c+80>>2]=d;f[c+84>>2]=b;y=b;v=jb(u^d,b^x,1);b=B;d=b;f[c+40>>2]=v;f[c+44>>2]=b;u=c;fa=f[c+160>>2];ca=f[c+164>>2];b=E+S|0;E=q+V|0;if(E>>>0<q>>>0){b=b+1|0}q=b;p=E;x=b;n=jb(t^C,n^O,1);b=B;S=b;t=b;O=i;b=z+X|0;i=aa;C=i+W|0;if(C>>>0<i>>>0){b=b+1|0}i=C+n|0;b=b+t|0;t=i;z=m;m=i>>>0<C>>>0?b+1|0:b;i=jb(O^i,z^m,32);b=x;x=B;b=b+x|0;z=i+p|0;if(z>>>0<i>>>0){b=b+1|0}C=z;p=n^C;n=b;p=jb(p,S^b,40);O=B;b=m+O|0;m=p+t|0;if(m>>>0<t>>>0){b=b+1|0}t=m+fa|0;b=b+ca|0;W=t;z=t>>>0<m>>>0?b+1|0:b;S=jb(i^t,x^z,48);V=B;b=n+V|0;i=C+S|0;if(i>>>0<C>>>0){b=b+1|0}t=i;f[u+88>>2]=i;f[u+92>>2]=b;n=b;x=jb(p^i,b^O,1);b=B;m=b;f[c+48>>2]=x;f[c+52>>2]=b;i=Z;C=la;b=f[c+192>>2];p=f[c+196>>2];E=jb(E^K,q^_,1);O=b;q=B;b=D+q|0;K=r+E|0;if(K>>>0<r>>>0){b=b+1|0}r=K;K=O+r|0;b=b+p|0;b=K>>>0<r>>>0?b+1|0:b;r=C;C=b;b=r+b|0;r=i;i=K;r=r+i|0;if(r>>>0<i>>>0){b=b+1|0}i=b;C=jb(e^K,C^L,32);D=B;b=w+D|0;K=j+C|0;if(K>>>0<j>>>0){b=b+1|0}e=K;j=b;K=jb(E^e,q^b,40);q=B;b=q+i|0;i=K;E=i+r|0;if(E>>>0<i>>>0){b=b+1|0}r=b;L=jb(C^E,D^b,48);C=B;b=j+C|0;i=e+L|0;if(i>>>0<e>>>0){b=b+1|0}j=i;w=b;i=jb(K^i,q^b,1);b=B;O=b;f[u+56>>2]=i;f[u+60>>2]=b;K=c;D=aa;q=X;e=F;p=J;_=f[c+148>>2];b=s+H|0;u=l+I|0;if(u>>>0<l>>>0){b=b+1|0}l=u;u=l+f[c+144>>2]|0;b=b+_|0;b=u>>>0<l>>>0?b+1|0:b;l=b;b=b+p|0;p=e;e=u;H=p+e|0;if(H>>>0<e>>>0){b=b+1|0}e=b;l=jb(o^u,l^U,32);p=B;b=h+p|0;u=k+l|0;if(u>>>0<k>>>0){b=b+1|0}k=u;o=b;u=jb(k^I,b^s,40);h=B;b=h+e|0;e=u;s=e+H|0;if(s>>>0<e>>>0){b=b+1|0}_=s;e=l^s;l=b;p=jb(e,p^b,48);fa=B;b=o+fa|0;e=k+p|0;if(e>>>0<k>>>0){b=b+1|0}s=e;k=u^e;u=b;H=jb(k,h^b,1);b=B;k=b;e=b;b=G+la|0;o=Z;h=o+P|0;if(h>>>0<o>>>0){b=b+1|0}o=h+H|0;b=b+e|0;e=o;o=e>>>0<h>>>0?b+1|0:b;b=o+q|0;D=e+D|0;if(D>>>0<e>>>0){b=b+1|0}q=D;h=b;D=jb(e^S,o^V,32);G=B;b=w+G|0;e=j+D|0;if(e>>>0<j>>>0){b=b+1|0}j=e;o=b;k=jb(e^H,b^k,40);b=h;h=B;b=b+h|0;e=k;H=e+q|0;if(H>>>0<e>>>0){b=b+1|0}q=H;H=b;e=jb(D^q,G^b,48);b=B;I=b;f[K+96>>2]=e;f[K+100>>2]=b;b=o+b|0;j=e+j|0;if(j>>>0<e>>>0){b=b+1|0}o=j;f[c+64>>2]=j;f[K+68>>2]=b;K=b;S=jb(k^j,b^h,1);b=B;h=b;f[c+32>>2]=S;f[c+36>>2]=b;k=c;G=f[c+200>>2];U=f[c+204>>2];j=v;w=d;b=z+$|0;z=T;D=z+W|0;if(D>>>0<z>>>0){b=b+1|0}z=v+D|0;b=b+d|0;d=z;z=C;C=d>>>0<v>>>0?b+1|0:b;v=jb(d^L,z^C,32);z=j;L=B;b=L+u|0;u=v;j=u+s|0;if(j>>>0<u>>>0){b=b+1|0}u=j;s=b;D=jb(z^j,b^w,40);w=G;G=B;b=C+G|0;j=d+D|0;if(j>>>0<d>>>0){b=b+1|0}C=j;j=w+j|0;b=b+U|0;w=j;z=j>>>0<C>>>0?b+1|0:b;U=jb(v^j,L^z,48);b=B;j=b;f[k+104>>2]=U;f[k+108>>2]=b;v=c;b=b+s|0;k=u+U|0;if(k>>>0<u>>>0){b=b+1|0}u=k;C=b;f[v+72>>2]=k;f[v+76>>2]=b;s=f[c+220>>2];b=m+r|0;k=x+E|0;if(k>>>0<E>>>0){b=b+1|0}d=k;k=d+f[c+216>>2]|0;b=b+s|0;b=k>>>0<d>>>0?b+1|0:b;d=k;E=f[c+152>>2];s=d+E|0;k=b;b=b+f[c+156>>2]|0;r=s;s=s>>>0<E>>>0?b+1|0:b;k=jb(d^p,k^fa,32);p=x;b=y;y=B;b=b+y|0;d=k;E=d+Q|0;if(E>>>0<d>>>0){b=b+1|0}x=E;E=b;m=jb(p^x,b^m,40);W=B;b=W+s|0;d=m+r|0;if(d>>>0<m>>>0){b=b+1|0}V=d;r=b;k=jb(k^d,y^b,48);b=B;L=b;f[v+112>>2]=k;f[v+116>>2]=b;s=c;d=jb(u^D,C^G,1);b=B;D=b;v=b;b=H+ea|0;H=R;q=H+q|0;if(q>>>0<H>>>0){b=b+1|0}H=q+d|0;b=b+v|0;b=H>>>0<q>>>0?b+1|0:b;y=f[c+144>>2];q=H+y|0;v=b;b=b+f[c+148>>2]|0;Q=q;q=q>>>0<y>>>0?b+1|0:b;y=d;G=f[c+132>>2];b=l+O|0;d=i;l=d+_|0;if(l>>>0<d>>>0){b=b+1|0}d=l+f[c+128>>2]|0;b=b+G|0;b=d>>>0<l>>>0?b+1|0:b;G=d;p=f[c+192>>2];l=d+p|0;d=b;b=b+f[c+196>>2]|0;b=l>>>0<p>>>0?b+1|0:b;p=l;l=b;d=jb(G^M,d^N,32);G=B;b=n+G|0;n=d+t|0;if(n>>>0<t>>>0){b=b+1|0}M=n;n=b;t=jb(i^M,O^b,40);O=H;_=B;b=_+l|0;i=t;l=i+p|0;if(l>>>0<i>>>0){b=b+1|0}H=b;G=jb(d^l,G^b,48);N=B;v=jb(O^G,N^v,32);b=B;O=b;i=v;p=b;b=E+L|0;d=k;E=d+x|0;if(E>>>0<d>>>0){b=b+1|0}d=E;x=y;y=b;b=b+p|0;E=d+i|0;if(E>>>0<d>>>0){b=b+1|0}p=E;i=D;D=b;E=jb(x^p,i^b,40);x=B;b=x+q|0;i=E;q=i+Q|0;if(q>>>0<i>>>0){b=b+1|0}fa=q;q=b;i=jb(v^fa,O^b,48);b=B;O=b;f[s+120>>2]=i;f[s+124>>2]=b;b=D+b|0;D=i+p|0;if(D>>>0<i>>>0){b=b+1|0}v=c;P=D;f[c+80>>2]=D;f[c+84>>2]=b;D=b;Q=jb(E^P,b^x,1);b=B;s=b;f[c+40>>2]=Q;f[c+44>>2]=b;E=Y;p=ha;b=f[c+232>>2];x=f[c+236>>2];m=jb(d^m,y^W,1);W=b;y=B;b=z+y|0;d=m+w|0;if(d>>>0<w>>>0){b=b+1|0}w=d;d=W+d|0;b=b+x|0;b=d>>>0<w>>>0?b+1|0:b;w=b;b=b+p|0;z=d+E|0;if(z>>>0<d>>>0){b=b+1|0}p=z;E=b;z=m;e=jb(e^d,w^I,32);b=B;x=b;d=e;w=b;b=n+N|0;m=G+M|0;if(m>>>0<G>>>0){b=b+1|0}n=w;w=b;b=n+b|0;n=d;d=m;n=n+d|0;if(n>>>0<d>>>0){b=b+1|0}d=n^z;z=b;d=jb(d,b^y,40);I=B;b=I+E|0;y=d+p|0;if(y>>>0<d>>>0){b=b+1|0}G=b;e=jb(e^y,x^b,48);p=B;b=z+p|0;E=e+n|0;if(E>>>0<n>>>0){b=b+1|0}f[v+88>>2]=E;f[v+92>>2]=b;z=b;v=jb(d^E,I^b,1);b=B;d=b;f[c+48>>2]=v;f[c+52>>2]=b;n=c;b=f[c+164>>2];x=b;I=f[c+160>>2];W=I;M=b;N=K;w=jb(m^t,w^_,1);b=B;_=b;K=b;b=r+ja|0;t=ga;m=t+V|0;if(m>>>0<t>>>0){b=b+1|0}t=m+w|0;b=b+K|0;b=t>>>0<m>>>0?b+1|0:b;m=b;K=jb(t^U,b^j,32);b=N;N=B;b=b+N|0;j=o;o=K;j=j+o|0;if(j>>>0<o>>>0){b=b+1|0}o=j;j=b;r=jb(w^o,_^b,40);w=B;b=m+w|0;m=r+t|0;if(m>>>0<t>>>0){b=b+1|0}t=m+W|0;b=b+M|0;W=t;m=t>>>0<m>>>0?b+1|0:b;M=jb(K^t,N^m,48);V=B;b=j+V|0;K=o+M|0;if(K>>>0<o>>>0){b=b+1|0}t=K;K=jb(r^t,b^w,1);o=B;N=o;f[n+56>>2]=K;f[n+60>>2]=o;o=c;U=f[c+144>>2];_=f[c+148>>2];j=b;n=e;r=ba;w=ka;b=h+J|0;e=F;ca=e+S|0;if(ca>>>0<e>>>0){b=b+1|0}e=l+ca|0;b=b+H|0;b=e>>>0<l>>>0?b+1|0:b;l=b;b=b+w|0;H=e+r|0;if(H>>>0<e>>>0){b=b+1|0}r=b;l=jb(e^k,l^L,32);w=B;b=C+w|0;e=l+u|0;if(e>>>0<u>>>0){b=b+1|0}k=b;u=jb(e^S,b^h,40);b=r;r=B;b=b+r|0;h=u;C=h+H|0;if(C>>>0<h>>>0){b=b+1|0}L=C;C=b;H=jb(l^L,w^b,48);w=B;b=k+w|0;k=e+H|0;if(k>>>0<e>>>0){b=b+1|0}e=k;k=b;h=jb(u^e,r^b,1);b=B;l=b;u=b;b=q+J|0;J=F+fa|0;if(J>>>0<F>>>0){b=b+1|0}F=J+h|0;b=b+u|0;b=F>>>0<J>>>0?b+1|0:b;J=F;u=b;F=jb(F^n,b^p,32);n=B;b=n+j|0;j=t;t=F;j=j+t|0;if(j>>>0<t>>>0){b=b+1|0}r=j;t=b;h=jb(h^j,l^b,40);l=B;b=u+l|0;u=h+J|0;if(u>>>0<J>>>0){b=b+1|0}J=u+U|0;b=b+_|0;q=J;j=q>>>0<u>>>0?b+1|0:b;u=jb(F^q,n^j,48);b=B;S=b;f[o+96>>2]=u;f[o+100>>2]=b;b=t+b|0;o=u+r|0;if(o>>>0<u>>>0){b=b+1|0}F=c;U=o;f[c+64>>2]=o;f[c+68>>2]=b;n=b;p=jb(h^o,l^b,1);b=B;o=b;f[c+32>>2]=p;f[c+36>>2]=b;t=f[c+196>>2];b=s+G|0;J=y+Q|0;if(J>>>0<y>>>0){b=b+1|0}h=J;J=h+f[c+192>>2]|0;b=b+t|0;b=J>>>0<h>>>0?b+1|0:b;t=b;b=b+x|0;h=J;l=h+I|0;if(l>>>0<h>>>0){b=b+1|0}h=b;t=jb(J^M,t^V,32);r=B;b=k+r|0;J=e+t|0;if(J>>>0<e>>>0){b=b+1|0}y=J;k=b;J=jb(y^Q,b^s,40);G=B;b=G+h|0;e=J;h=e+l|0;if(h>>>0<e>>>0){b=b+1|0}x=h;s=b;e=jb(t^h,r^b,48);b=B;I=b;f[F+104>>2]=e;f[F+108>>2]=b;b=k+b|0;t=e+y|0;if(t>>>0<e>>>0){b=b+1|0}k=t;t=b;f[F+72>>2]=k;f[F+76>>2]=b;l=Z;r=la;b=m+ha|0;h=Y;m=h+W|0;if(m>>>0<h>>>0){b=b+1|0}h=v;m=h+m|0;b=b+d|0;b=m>>>0<h>>>0?b+1|0:b;h=m;m=b;b=b+r|0;r=h+l|0;if(r>>>0<h>>>0){b=b+1|0}l=b;h=jb(h^H,m^w,32);H=B;b=H+D|0;m=h+P|0;if(m>>>0<h>>>0){b=b+1|0}Q=m;y=d;d=b;m=jb(m^v,y^b,40);P=B;b=P+l|0;l=m+r|0;if(l>>>0<m>>>0){b=b+1|0}r=b;v=jb(h^l,H^b,48);b=B;M=b;f[F+112>>2]=v;f[F+116>>2]=b;h=c;W=f[c+216>>2];V=f[c+220>>2];y=jb(k^J,t^G,1);b=B;G=b;F=b;b=j+X|0;J=aa;j=J+q|0;if(j>>>0<J>>>0){b=b+1|0}J=j+y|0;b=b+F|0;F=J;b=F>>>0<j>>>0?b+1|0:b;H=b;j=F;w=b;D=ba;q=ka;b=C+ja|0;J=ga;C=J+L|0;if(C>>>0<J>>>0){b=b+1|0}J=K;C=J+C|0;b=b+N|0;b=C>>>0<J>>>0?b+1|0:b;J=C;C=b;b=b+q|0;q=D+J|0;if(q>>>0<J>>>0){b=b+1|0}L=q;D=b;q=jb(i^J,C^O,32);O=B;b=z+O|0;J=q+E|0;if(J>>>0<E>>>0){b=b+1|0}_=J;C=b;i=jb(K^J,N^b,40);fa=B;b=fa+D|0;K=i+L|0;if(K>>>0<i>>>0){b=b+1|0}E=K;z=b;D=jb(q^E,O^b,48);L=B;J=jb(D^j,L^w,32);b=B;N=b;K=J;w=b;b=d+M|0;j=v;d=j+Q|0;if(d>>>0<j>>>0){b=b+1|0}j=d;d=b;b=b+w|0;w=j+K|0;if(w>>>0<j>>>0){b=b+1|0}Q=w;w=b;q=jb(y^Q,G^b,40);y=B;b=H+y|0;K=q+F|0;if(K>>>0<F>>>0){b=b+1|0}F=K+W|0;b=b+V|0;O=F;H=F>>>0<K>>>0?b+1|0:b;K=jb(J^F,N^H,48);b=B;N=b;f[h+120>>2]=K;f[h+124>>2]=b;b=w+b|0;J=K;h=J+Q|0;if(h>>>0<J>>>0){b=b+1|0}Q=h;f[c+80>>2]=h;f[c+84>>2]=b;w=b;G=jb(q^h,b^y,1);b=B;h=b;f[c+40>>2]=G;f[c+44>>2]=b;J=c;q=T;y=$;b=f[c+200>>2];W=f[c+204>>2];F=jb(m^j,d^P,1);d=b;j=B;b=j+s|0;m=F;s=m+x|0;if(s>>>0<m>>>0){b=b+1|0}m=d+s|0;b=b+W|0;b=m>>>0<s>>>0?b+1|0:b;s=b;b=b+y|0;d=m;q=d+q|0;if(q>>>0<d>>>0){b=b+1|0}y=q;d=b;q=F;F=jb(m^u,s^S,32);b=B;x=b;m=F;s=b;b=C+L|0;u=D+_|0;if(u>>>0<D>>>0){b=b+1|0}C=b;b=b+s|0;s=m;m=u;s=s+m|0;if(s>>>0<m>>>0){b=b+1|0}m=j;j=b;m=jb(s^q,m^b,40);L=B;b=L+d|0;d=m;D=d+y|0;if(D>>>0<d>>>0){b=b+1|0}d=D;D=b;y=jb(F^d,x^b,48);P=B;b=j+P|0;F=s+y|0;if(F>>>0<s>>>0){b=b+1|0}S=F;f[J+88>>2]=F;f[J+92>>2]=b;q=b;x=jb(m^F,b^L,1);b=B;J=b;f[c+48>>2]=x;f[c+52>>2]=b;F=c;m=R;j=ea;b=f[c+152>>2];s=f[c+156>>2];C=jb(i^u,C^fa,1);W=b;L=B;b=r+L|0;u=l+C|0;if(u>>>0<l>>>0){b=b+1|0}i=u;u=W+i|0;b=b+s|0;b=u>>>0<i>>>0?b+1|0:b;i=b;b=b+j|0;j=m;m=u;j=j+m|0;if(j>>>0<m>>>0){b=b+1|0}s=j;m=b;u=jb(e^u,i^I,32);l=B;b=l+n|0;i=u;e=i+U|0;if(e>>>0<i>>>0){b=b+1|0}j=b;i=jb(C^e,L^b,40);C=B;b=C+m|0;s=i+s|0;if(s>>>0<i>>>0){b=b+1|0}n=b;U=jb(u^s,l^b,48);W=B;b=j+W|0;u=e+U|0;if(u>>>0<e>>>0){b=b+1|0}e=u;j=i^e;i=b;I=jb(j,C^b,1);b=B;L=b;f[F+56>>2]=I;f[F+60>>2]=b;m=ga;j=ja;u=f[c+132>>2];b=u+H|0;C=f[c+128>>2];l=C;r=l+O|0;if(r>>>0<l>>>0){b=b+1|0}H=r;l=b;r=u;O=f[c+236>>2];b=o+z|0;u=p+E|0;if(u>>>0<E>>>0){b=b+1|0}E=u;u=u+f[c+232>>2]|0;b=b+O|0;b=u>>>0<E>>>0?b+1|0:b;E=b;b=b+r|0;r=u+C|0;if(r>>>0<u>>>0){b=b+1|0}z=r;C=b;r=jb(u^v,E^M,32);E=p;p=B;b=t+p|0;v=k+r|0;if(v>>>0<k>>>0){b=b+1|0}u=v;k=b;v=jb(E^u,b^o,40);t=B;b=t+C|0;o=v;C=o+z|0;if(C>>>0<o>>>0){b=b+1|0}E=C;o=r^C;r=b;p=jb(o,p^b,48);O=B;b=k+O|0;k=u+p|0;if(k>>>0<u>>>0){b=b+1|0}o=b;v=jb(v^k,t^b,1);t=B;b=t+l|0;u=v;C=u+H|0;if(C>>>0<u>>>0){b=b+1|0}u=C;l=j;j=b;b=l+b|0;l=m;m=u;C=l+m|0;if(C>>>0<m>>>0){b=b+1|0}m=b;j=jb(u^y,j^P,32);l=v;z=B;b=i+z|0;v=e+j|0;if(v>>>0<e>>>0){b=b+1|0}e=v;i=b;u=jb(l^e,b^t,40);b=m;m=B;b=b+m|0;t=u+C|0;if(t>>>0<u>>>0){b=b+1|0}l=t;H=b;v=jb(j^l,z^b,48);b=B;M=b;f[F+96>>2]=v;f[F+100>>2]=b;b=i+b|0;i=v;e=i+e|0;if(e>>>0<i>>>0){b=b+1|0}t=e;f[c+64>>2]=e;f[F+68>>2]=b;F=b;u=jb(u^e,b^m,1);b=B;m=b;f[c+32>>2]=u;f[c+36>>2]=b;e=c;j=f[c+148>>2];b=h+D|0;i=d+G|0;if(i>>>0<d>>>0){b=b+1|0}C=i;i=i+f[c+144>>2]|0;b=b+j|0;b=i>>>0<C>>>0?b+1|0:b;C=i;d=f[c+152>>2];j=i+d|0;i=b;b=b+f[c+156>>2]|0;z=j;j=j>>>0<d>>>0?b+1|0:b;d=jb(C^U,i^W,32);D=G;y=B;b=o+y|0;i=d+k|0;if(i>>>0<k>>>0){b=b+1|0}G=i;C=b;o=jb(D^i,b^h,40);P=B;b=P+j|0;i=o;k=i+z|0;if(k>>>0<i>>>0){b=b+1|0}z=k;D=b;i=jb(d^k,y^b,48);b=B;h=b;f[e+104>>2]=i;f[e+108>>2]=b;k=c;b=b+C|0;e=i;j=e+G|0;if(j>>>0<e>>>0){b=b+1|0}e=j;j=b;f[k+72>>2]=e;f[k+76>>2]=b;C=c;d=ba;y=ka;G=f[c+164>>2];b=n+J|0;k=s+x|0;if(k>>>0<s>>>0){b=b+1|0}s=k;k=k+f[c+160>>2]|0;b=b+G|0;b=k>>>0<s>>>0?b+1|0:b;s=b;b=b+y|0;n=d;d=k;n=n+d|0;if(n>>>0<d>>>0){b=b+1|0}y=n;d=b;n=J;J=jb(k^p,s^O,32);G=B;b=G+w|0;k=J;s=k+Q|0;if(s>>>0<k>>>0){b=b+1|0}Q=s;k=n;n=b;s=jb(s^x,k^b,40);O=B;b=O+d|0;k=s;d=k+y|0;if(d>>>0<k>>>0){b=b+1|0}w=d;y=b;k=jb(J^d,G^b,48);b=B;U=b;f[C+112>>2]=k;f[C+116>>2]=b;b=f[c+192>>2];G=f[c+196>>2];o=jb(e^o,j^P,1);p=b;d=B;b=H+d|0;J=l+o|0;if(J>>>0<l>>>0){b=b+1|0}l=J;J=p+l|0;b=b+G|0;b=J>>>0<l>>>0?b+1|0:b;l=J;G=f[c+200>>2];H=l+G|0;J=b;b=b+f[c+204>>2]|0;P=H;H=H>>>0<G>>>0?b+1|0:b;G=d;p=J;d=Y;x=ha;b=L+la|0;J=Z;W=J+I|0;if(W>>>0<J>>>0){b=b+1|0}J=E+W|0;b=b+r|0;b=J>>>0<E>>>0?b+1|0:b;E=b;b=b+x|0;r=d;d=J;r=r+d|0;if(r>>>0<d>>>0){b=b+1|0}x=r;r=b;J=jb(J^K,E^N,32);E=B;b=E+q|0;d=J+S|0;if(d>>>0<J>>>0){b=b+1|0}N=d;q=b;d=jb(I^d,L^b,40);K=l;S=B;b=S+r|0;l=d+x|0;if(l>>>0<d>>>0){b=b+1|0}L=l;r=b;l=jb(J^l,E^b,48);W=B;J=jb(K^l,W^p,32);b=B;x=b;K=J;p=b;b=n+U|0;n=k+Q|0;if(n>>>0<k>>>0){b=b+1|0}E=n;I=o;o=p;p=b;b=o+b|0;o=n+K|0;if(o>>>0<n>>>0){b=b+1|0}Q=o;n=b;o=jb(I^o,b^G,40);V=B;b=V+H|0;H=o+P|0;if(H>>>0<o>>>0){b=b+1|0}P=H;H=b;K=jb(J^P,x^b,48);b=B;I=b;f[C+120>>2]=K;f[C+124>>2]=b;b=n+b|0;C=K;n=C+Q|0;if(n>>>0<C>>>0){b=b+1|0}J=c;C=n;f[c+80>>2]=n;f[c+84>>2]=b;G=b;o=jb(o^n,V^b,1);b=B;n=b;f[c+40>>2]=o;f[c+44>>2]=b;b=f[c+208>>2];x=f[c+212>>2];s=jb(s^E,p^O,1);p=b;E=B;b=D+E|0;D=s+z|0;if(D>>>0<z>>>0){b=b+1|0}z=p+D|0;b=b+x|0;b=z>>>0<D>>>0?b+1|0:b;x=z;p=f[c+216>>2];D=x+p|0;z=b;b=b+f[c+220>>2]|0;Q=D;D=D>>>0<p>>>0?b+1|0:b;p=s;v=jb(v^x,z^M,32);b=B;M=b;z=v;x=b;b=q+W|0;s=l+N|0;if(s>>>0<l>>>0){b=b+1|0}q=b;b=b+x|0;l=s;z=l+z|0;if(z>>>0<l>>>0){b=b+1|0}l=z;z=b;E=jb(l^p,b^E,40);x=B;b=x+D|0;p=E+Q|0;if(p>>>0<E>>>0){b=b+1|0}N=p;D=b;v=jb(v^p,M^b,48);p=B;b=z+p|0;z=l+v|0;if(z>>>0<l>>>0){b=b+1|0}l=z;f[J+88>>2]=l;f[J+92>>2]=b;z=E;E=b;x=jb(z^l,b^x,1);b=B;M=b;f[c+48>>2]=x;f[c+52>>2]=b;Q=f[c+232>>2];O=f[c+236>>2];z=F;F=jb(d^s,q^S,1);q=i;d=B;b=d+ea|0;i=F;s=i+R|0;if(s>>>0<i>>>0){b=b+1|0}i=s+w|0;b=b+y|0;s=i;y=h;h=i>>>0<w>>>0?b+1|0:b;i=jb(q^i,y^h,32);w=B;b=w+z|0;z=i+t|0;if(z>>>0<i>>>0){b=b+1|0}t=z;q=F^t;F=b;d=jb(q,d^b,40);z=B;b=h+z|0;h=d+s|0;if(h>>>0<s>>>0){b=b+1|0}s=h;h=h+Q|0;b=b+O|0;b=h>>>0<s>>>0?b+1|0:b;s=b;q=jb(i^h,w^b,48);S=B;b=F+S|0;F=q+t|0;if(F>>>0<t>>>0){b=b+1|0}i=F;F=jb(d^i,b^z,1);t=B;y=t;f[J+56>>2]=F;f[J+60>>2]=t;Q=f[c+208>>2];O=f[c+212>>2];t=b;d=v;w=aa;z=X;b=r+$|0;v=T;r=v+L|0;if(r>>>0<v>>>0){b=b+1|0}r=u+r|0;b=b+m|0;v=r;r=v>>>0<u>>>0?b+1|0:b;b=r+z|0;z=v+w|0;if(z>>>0<v>>>0){b=b+1|0}w=b;r=jb(k^v,r^U,32);k=u;U=B;b=j+U|0;v=e+r|0;if(v>>>0<e>>>0){b=b+1|0}u=v;e=b;v=jb(k^u,b^m,40);m=B;b=m+w|0;k=v;j=k+z|0;if(j>>>0<k>>>0){b=b+1|0}L=j;k=b;w=jb(r^j,U^b,48);U=B;b=e+U|0;e=u+w|0;if(e>>>0<u>>>0){b=b+1|0}j=v^e;v=b;m=jb(j,m^b,1);b=B;j=b;u=b;b=H+$|0;$=T+P|0;if($>>>0<T>>>0){b=b+1|0}T=$+m|0;b=b+u|0;b=T>>>0<$>>>0?b+1|0:b;$=T;u=b;T=jb(T^d,b^p,32);d=B;b=d+t|0;t=i;i=T;t=t+i|0;if(t>>>0<i>>>0){b=b+1|0}r=t;i=b;m=jb(m^t,j^b,40);j=B;b=u+j|0;u=m+$|0;if(u>>>0<$>>>0){b=b+1|0}$=u+Q|0;b=b+O|0;p=$;t=p>>>0<u>>>0?b+1|0:b;$=jb(T^p,d^t,48);b=B;H=b;f[J+96>>2]=$;f[J+100>>2]=b;b=i+b|0;J=$;u=J+r|0;if(u>>>0<J>>>0){b=b+1|0}T=c;f[c+64>>2]=u;f[c+68>>2]=b;d=m^u;m=b;r=jb(d,j^b,1);b=B;z=b;f[c+32>>2]=r;f[c+36>>2]=b;i=f[c+164>>2];b=n+D|0;j=o+N|0;if(j>>>0<o>>>0){b=b+1|0}J=j+f[c+160>>2]|0;b=b+i|0;b=J>>>0<j>>>0?b+1|0:b;d=J;j=f[c+192>>2];i=d+j|0;J=b;b=b+f[c+196>>2]|0;D=i;j=i>>>0<j>>>0?b+1|0:b;d=jb(d^q,J^S,32);q=B;b=v+q|0;J=e+d|0;if(J>>>0<e>>>0){b=b+1|0}e=J;v=b;i=jb(e^o,b^n,40);N=B;b=N+j|0;o=i+D|0;if(o>>>0<i>>>0){b=b+1|0}j=b;J=jb(d^o,q^b,48);b=B;n=b;f[T+104>>2]=J;f[T+108>>2]=b;b=b+v|0;v=J;e=v+e|0;if(e>>>0<v>>>0){b=b+1|0}v=e;e=b;f[T+72>>2]=v;f[T+76>>2]=b;d=aa;D=f[c+204>>2];b=s+M|0;aa=h+x|0;if(aa>>>0<h>>>0){b=b+1|0}h=aa;aa=h+f[c+200>>2]|0;b=b+D|0;b=aa>>>0<h>>>0?b+1|0:b;h=X;X=b;b=h+b|0;h=aa;s=h+d|0;if(s>>>0<h>>>0){b=b+1|0}d=s;h=b;s=jb(w^aa,U^X,32);w=B;b=G+w|0;aa=s+C|0;if(aa>>>0<C>>>0){b=b+1|0}q=aa;C=b;X=jb(x^q,M^b,40);G=B;b=G+h|0;h=d+X|0;if(h>>>0<X>>>0){b=b+1|0}d=s^h;s=b;aa=jb(d,w^b,48);b=B;w=b;f[T+112>>2]=aa;f[T+116>>2]=b;d=R;D=jb(i^v,e^N,1);b=B;x=b;R=b;b=t+ja|0;ja=p+ga|0;if(ja>>>0<ga>>>0){b=b+1|0}ga=ja+D|0;b=b+R|0;R=ga;ga=R>>>0<ja>>>0?b+1|0:b;b=ga+ea|0;ja=d+R|0;if(ja>>>0<R>>>0){b=b+1|0}d=ja;ea=b;ja=R;i=f[c+236>>2];b=k+y|0;k=F+L|0;if(k>>>0<F>>>0){b=b+1|0}R=k+f[c+232>>2]|0;b=b+i|0;b=R>>>0<k>>>0?b+1|0:b;i=la;la=b;b=i+b|0;i=R+Z|0;if(i>>>0<R>>>0){b=b+1|0}k=i;i=b;K=jb(K^R,I^la,32);t=B;b=E+t|0;R=l+K|0;if(R>>>0<l>>>0){b=b+1|0}E=R;la=b;Z=jb(F^E,y^b,40);p=ja;l=B;b=l+i|0;R=Z;F=R+k|0;if(F>>>0<R>>>0){b=b+1|0}ja=F;i=K^F;K=b;i=jb(i,t^b,48);y=B;R=jb(p^i,y^ga,32);b=B;p=b;F=R;k=b;b=C+w|0;t=q+aa|0;if(t>>>0<aa>>>0){b=b+1|0}ga=t;q=k;k=b;b=q+b|0;q=F;F=t;t=q+t|0;if(t>>>0<F>>>0){b=b+1|0}C=t;t=b;F=jb(D^C,x^b,40);D=B;b=D+ea|0;d=d+F|0;if(d>>>0<F>>>0){b=b+1|0}f[T>>2]=d;f[T+4>>2]=b;R=jb(R^d,b^p,48);b=B;f[c+120>>2]=R;f[c+124>>2]=b;b=t+b|0;ea=C+R|0;if(ea>>>0<R>>>0){b=b+1|0}R=ea;f[c+80>>2]=R;f[T+84>>2]=b;f[c+40>>2]=jb(F^R,b^D,1);f[c+44>>2]=B;R=c;b=f[c+128>>2];ea=f[c+132>>2];ga=jb(X^ga,k^G,1);d=b;T=B;b=j+T|0;F=o+ga|0;if(F>>>0<o>>>0){b=b+1|0}X=F;F=d+F|0;b=b+ea|0;b=F>>>0<X>>>0?b+1|0:b;o=F;X=f[c+144>>2];ea=o+X|0;F=b;b=b+f[c+148>>2]|0;t=ea;ea=t>>>0<X>>>0?b+1|0:b;X=ga;k=T;ga=jb(o^$,F^H,32);b=B;o=b;F=ga;$=b;b=y+la|0;T=i+E|0;if(T>>>0<i>>>0){b=b+1|0}d=X;la=b;b=b+$|0;i=F;F=T;X=i+F|0;if(X>>>0<F>>>0){b=b+1|0}j=X;X=b;F=jb(d^j,b^k,40);C=B;b=C+ea|0;$=t+F|0;if($>>>0<F>>>0){b=b+1|0}f[R+8>>2]=$;f[R+12>>2]=b;ea=b;ga=jb($^ga,b^o,48);b=B;i=b;f[c+96>>2]=ga;f[c+100>>2]=b;b=b+X|0;X=ga;k=X+j|0;if(k>>>0<X>>>0){b=b+1|0}f[c+88>>2]=k;f[R+92>>2]=b;d=F;F=b;o=jb(d^k,b^C,1);b=B;t=b;f[c+48>>2]=o;f[c+52>>2]=b;X=Y;b=f[c+216>>2];j=f[c+220>>2];la=jb(T^Z,l^la,1);d=b;Z=B;b=s+Z|0;Y=h+la|0;if(Y>>>0<h>>>0){b=b+1|0}T=Y;Y=d+T|0;b=b+j|0;b=Y>>>0<T>>>0?b+1|0:b;T=b;b=b+ha|0;X=X+Y|0;if(X>>>0<Y>>>0){b=b+1|0}h=X;ha=b;X=jb(J^Y,n^T,32);J=B;b=m+J|0;Y=u+X|0;if(Y>>>0<u>>>0){b=b+1|0}u=Y;d=Z;Z=b;Y=jb(u^la,d^b,40);m=B;b=m+ha|0;T=Y;ha=T+h|0;if(ha>>>0<T>>>0){b=b+1|0}la=ha;f[R+16>>2]=ha;f[R+20>>2]=b;T=b;R=jb(X^ha,b^J,48);b=B;X=b;f[c+104>>2]=R;f[c+108>>2]=b;b=b+Z|0;J=u+R|0;if(J>>>0<R>>>0){b=b+1|0}f[c+64>>2]=J;f[c+68>>2]=b;Z=b;u=jb(J^Y,b^m,1);b=B;h=b;f[c+56>>2]=u;f[c+60>>2]=b;Y=c;m=f[c+152>>2];j=f[c+156>>2];b=z+ka|0;ha=r+ba|0;if(ha>>>0<ba>>>0){b=b+1|0}ba=ha+ja|0;b=b+K|0;b=ba>>>0<ja>>>0?b+1|0:b;d=aa^ba;aa=b;ja=jb(d,w^b,32);K=B;b=e+K|0;ha=v+ja|0;if(ha>>>0<v>>>0){b=b+1|0}e=ha;ha=b;ka=jb(r^e,z^b,40);d=m;m=B;b=aa+m|0;aa=ba+ka|0;if(aa>>>0<ba>>>0){b=b+1|0}ba=d+aa|0;b=b+j|0;v=ba;f[Y+24>>2]=v;b=v>>>0<aa>>>0?b+1|0:b;f[Y+28>>2]=b;ba=b;Y=jb(v^ja,b^K,48);b=B;ja=b;f[c+112>>2]=Y;f[c+116>>2]=b;b=ha+b|0;K=e+Y|0;if(K>>>0<Y>>>0){b=b+1|0}ha=K;f[c+72>>2]=K;f[c+76>>2]=b;aa=jb(K^ka,b^m,1);ka=B;K=ka;f[c+32>>2]=aa;f[c+36>>2]=K;Z=Z^(f[c+4>>2]^f[a+4>>2]);f[a>>2]=J^(f[c>>2]^f[a>>2]);f[a+4>>2]=Z;b=b^(f[a+12>>2]^ea);f[a+8>>2]=ha^(f[a+8>>2]^$);f[a+12>>2]=b;b=f[c+80>>2];Z=f[c+84>>2];ba=F^(f[a+28>>2]^ba);f[a+24>>2]=k^(f[a+24>>2]^v);f[a+28>>2]=ba;ba=i^(f[a+36>>2]^K);f[a+32>>2]=ga^(f[a+32>>2]^aa);f[a+36>>2]=ba;ga=f[a+20>>2]^T^Z;f[a+16>>2]=f[a+16>>2]^la^b;f[a+20>>2]=ga;b=f[c+40>>2];ga=f[c+44>>2];ba=ja^(f[a+52>>2]^t);f[a+48>>2]=Y^(f[a+48>>2]^o);f[a+52>>2]=ba;Y=X^(f[a+44>>2]^ga);f[a+40>>2]=R^(f[a+40>>2]^b);f[a+44>>2]=Y;b=f[c+124>>2]^(f[a+60>>2]^h);f[a+56>>2]=f[c+120>>2]^(f[a+56>>2]^u);f[a+60>>2]=b;A=c+256|0}function Qa(a,b,c,d){var e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0;e=A-2832|0;A=e;m=-1;a:{if(g[d+63|0]>31){break a}if(!ia(e+160|0,c)){break a}R(e+640|0);N(e+640|0,d,32);N(e+640|0,c,32);N(e+640|0,a,b);Q(e+640|0,e+96|0);V(e+48|0,e+96|0,64);V(e,d+32|0,32);pa(e+2176|0,e+48|0,5);pa(e+1920|0,e,7);_(e+2432|0,e+160|0);a=e+2552|0;M(e+480|0,e+2432|0,a);b=e+2472|0;c=e+2512|0;M(e+520|0,b,c);o=e+560|0;M(o,c,a);v=e+600|0;M(v,e+2432|0,b);a=f[e+200>>2];b=f[e+160>>2];c=(a-b|0)+134217690|0;f[e+640>>2]=c&67108863;h=c>>>26|0;c=f[e+204>>2];m=f[e+164>>2];s=((h+c|0)-m|0)+67108862|0;f[e+644>>2]=s&33554431;t=f[e+208>>2];x=f[e+168>>2];s=(t-x+(s>>>25)|0)+134217726|0;f[e+648>>2]=s&67108863;y=f[e+212>>2];j=f[e+172>>2];s=(y-j+(s>>>26)|0)+67108862|0;f[e+652>>2]=s&33554431;h=f[e+216>>2];i=f[e+176>>2];f[e+656>>2]=(h-i+(s>>>25)|0)+134217726;s=f[e+180>>2];n=f[e+220>>2];f[e+700>>2]=s+n;f[e+696>>2]=h+i;f[e+692>>2]=j+y;f[e+688>>2]=t+x;f[e+684>>2]=c+m;c=f[e+196>>2];m=f[e+236>>2];f[e+716>>2]=c+m;t=f[e+192>>2];x=f[e+232>>2];f[e+712>>2]=t+x;y=f[e+188>>2];j=f[e+228>>2];f[e+708>>2]=y+j;h=f[e+184>>2];i=f[e+224>>2];f[e+704>>2]=h+i;f[e+660>>2]=(n-s|0)+67108862;f[e+680>>2]=a+b;f[e+664>>2]=(i-h|0)+134217726;f[e+668>>2]=(j-y|0)+67108862;f[e+672>>2]=(x-t|0)+134217726;f[e+676>>2]=(m-c|0)+67108862;f[e+720>>2]=f[e+240>>2];f[e+724>>2]=f[e+244>>2];f[e+728>>2]=f[e+248>>2];f[e+732>>2]=f[e+252>>2];f[e+736>>2]=f[e+256>>2];f[e+740>>2]=f[e+260>>2];f[e+744>>2]=f[e+264>>2];f[e+748>>2]=f[e+268>>2];f[e+752>>2]=f[e+272>>2];f[e+756>>2]=f[e+276>>2];M(e+760|0,e+280|0,29616);c=0;while(1){a=f[e+520>>2];b=f[e+480>>2];m=(a-b|0)+134217690|0;f[e+2784>>2]=m&67108863;h=m>>>26|0;m=f[e+524>>2];s=f[e+484>>2];t=((h+m|0)-s|0)+67108862|0;f[e+2788>>2]=t&33554431;x=f[e+528>>2];y=f[e+488>>2];t=(x-y+(t>>>25)|0)+134217726|0;f[e+2792>>2]=t&67108863;j=f[e+532>>2];h=f[e+492>>2];t=(j-h+(t>>>26)|0)+67108862|0;f[e+2796>>2]=t&33554431;i=f[e+536>>2];n=f[e+496>>2];f[e+2800>>2]=(i-n+(t>>>25)|0)+134217726;t=f[e+540>>2];p=f[e+500>>2];f[e+2804>>2]=(t-p|0)+67108862;q=f[e+544>>2];r=f[e+504>>2];f[e+2808>>2]=(q-r|0)+134217726;k=f[e+548>>2];u=f[e+508>>2];f[e+2812>>2]=(k-u|0)+67108862;w=f[e+552>>2];z=f[e+512>>2];f[e+2816>>2]=(w-z|0)+134217726;B=f[e+556>>2];C=f[e+516>>2];f[e+2820>>2]=(B-C|0)+67108862;f[e+2772>>2]=B+C;f[e+2768>>2]=w+z;f[e+2764>>2]=k+u;f[e+2760>>2]=q+r;f[e+2756>>2]=p+t;f[e+2752>>2]=i+n;f[e+2748>>2]=h+j;f[e+2744>>2]=x+y;f[e+2740>>2]=m+s;f[e+2736>>2]=a+b;a=(e+640|0)+l(c,160)|0;M(e+2784|0,e+2784|0,a);M(e+2432|0,e+2736|0,a+40|0);b=f[e+2784>>2];m=f[e+2432>>2];f[e+320>>2]=b+m;s=f[e+2788>>2];t=f[e+2436>>2];f[e+324>>2]=s+t;x=f[e+2792>>2];y=f[e+2440>>2];f[e+328>>2]=x+y;j=f[e+2796>>2];h=f[e+2444>>2];f[e+332>>2]=j+h;i=f[e+2800>>2];n=f[e+2448>>2];f[e+336>>2]=i+n;p=f[e+2804>>2];q=f[e+2452>>2];f[e+340>>2]=p+q;f[e+2452>>2]=(q-p|0)+67108862;b=(m-b|0)+134217690|0;f[e+2432>>2]=b&67108863;m=f[e+2808>>2];p=f[e+2456>>2];f[e+344>>2]=m+p;q=f[e+2812>>2];r=f[e+2460>>2];f[e+348>>2]=q+r;k=f[e+2816>>2];u=f[e+2464>>2];f[e+352>>2]=k+u;w=f[e+2820>>2];z=f[e+2468>>2];f[e+356>>2]=w+z;f[e+2460>>2]=(r-q|0)+67108862;f[e+2464>>2]=(u-k|0)+134217726;f[e+2468>>2]=(z-w|0)+67108862;f[e+2456>>2]=(p-m|0)+134217726;b=((t+(b>>>26|0)|0)-s|0)+67108862|0;f[e+2436>>2]=b&33554431;b=(y-x+(b>>>25)|0)+134217726|0;f[e+2440>>2]=b&67108863;b=(h-j+(b>>>26)|0)+67108862|0;f[e+2444>>2]=b&33554431;f[e+2448>>2]=(n-i+(b>>>25)|0)+134217726;M(e+2688|0,v,a+120|0);M(e+2592|0,o,a+80|0);p=f[e+2596>>2];b=f[e+2692>>2];m=f[e+2592>>2]<<1;f[e+2592>>2]=m;s=f[e+2688>>2];t=f[e+2628>>2]<<1;f[e+2628>>2]=t;x=f[e+2624>>2]<<1;f[e+2624>>2]=x;y=f[e+2620>>2]<<1;f[e+2620>>2]=y;j=f[e+2616>>2]<<1;f[e+2616>>2]=j;h=f[e+2612>>2]<<1;f[e+2612>>2]=h;i=f[e+2608>>2]<<1;f[e+2608>>2]=i;n=f[e+2604>>2]<<1;f[e+2604>>2]=n;q=f[e+2600>>2];p=p<<1;r=m+s|0;k=p+b+(r>>>26)|0;f[e+2644>>2]=k&33554431;u=f[e+2696>>2];q=q<<1;k=u+q+(k>>>25)|0;f[e+2648>>2]=k&67108863;w=f[e+2700>>2];k=w+n+(k>>>26)|0;f[e+2652>>2]=k&33554431;z=f[e+2704>>2];k=z+i+(k>>>25)|0;f[e+2656>>2]=k&67108863;B=f[e+2708>>2];k=B+h+(k>>>26)|0;f[e+2660>>2]=k&33554431;C=f[e+2712>>2];k=C+j+(k>>>25)|0;f[e+2664>>2]=k&67108863;D=f[e+2716>>2];k=D+y+(k>>>26)|0;f[e+2668>>2]=k&33554431;E=f[e+2720>>2];k=E+x+(k>>>25)|0;f[e+2672>>2]=k&67108863;F=f[e+2724>>2];k=F+t+(k>>>26)|0;f[e+2676>>2]=k&33554431;f[e+2640>>2]=l(k>>>25|0,19)+(r&67108863);k=t-F|0;r=x-E|0;D=y-D|0;p=p-b|0;b=(m-s|0)+268435380|0;m=(p+(b>>>26|0)|0)+134217724|0;s=(q-u+(m>>>25)|0)+268435452|0;t=(n-w+(s>>>26)|0)+134217724|0;x=(i-z+(t>>>25)|0)+268435452|0;y=(h-B+(x>>>26)|0)+134217724|0;j=(j-C+(y>>>25)|0)+268435452|0;h=(D+(j>>>26|0)|0)+134217724|0;i=(r+(h>>>25|0)|0)+268435452|0;n=(k+(i>>>26|0)|0)+134217724|0;f[e+2628>>2]=n&33554431;f[e+2624>>2]=i&67108863;f[e+2620>>2]=h&33554431;f[e+2616>>2]=j&67108863;f[e+2612>>2]=y&33554431;f[e+2608>>2]=x&67108863;f[e+2604>>2]=t&33554431;f[e+2600>>2]=s&67108863;f[e+2596>>2]=m&33554431;f[e+2592>>2]=l(n>>>25|0,19)+(b&67108863);M(a+200|0,e+2432|0,e+2592|0);M(a+160|0,e+320|0,e+2640|0);M(a+240|0,e+2640|0,e+2592|0);b=a+280|0;M(b,e+2432|0,e+320|0);y=f[a+160>>2];j=f[a+200>>2];m=(y-j|0)+134217690|0;f[a+160>>2]=m&67108863;h=f[a+164>>2];i=f[a+204>>2];m=((h+(m>>>26|0)|0)-i|0)+67108862|0;f[a+164>>2]=m&33554431;n=f[a+168>>2];p=f[a+208>>2];m=(n-p+(m>>>25)|0)+134217726|0;f[a+168>>2]=m&67108863;q=f[a+172>>2];r=f[a+212>>2];m=(q-r+(m>>>26)|0)+67108862|0;f[a+172>>2]=m&33554431;k=f[a+176>>2];u=f[a+216>>2];f[a+176>>2]=(k-u+(m>>>25)|0)+134217726;m=f[a+196>>2];s=f[a+192>>2];t=f[a+188>>2];x=f[a+184>>2];w=f[a+180>>2];z=f[a+220>>2];f[a+180>>2]=(w-z|0)+67108862;B=f[a+224>>2];f[a+184>>2]=(x-B|0)+134217726;C=f[a+228>>2];f[a+188>>2]=(t-C|0)+67108862;D=f[a+232>>2];f[a+192>>2]=(s-D|0)+134217726;f[a+216>>2]=k+u;f[a+204>>2]=h+i;f[a+208>>2]=n+p;f[a+212>>2]=q+r;f[a+220>>2]=w+z;f[a+224>>2]=x+B;f[a+228>>2]=t+C;f[a+232>>2]=s+D;s=f[a+236>>2];f[a+200>>2]=j+y;f[a+236>>2]=m+s;f[a+196>>2]=(m-s|0)+67108862;M(b,b,29616);c=c+1|0;if((c|0)!=7){continue}break}O(e+320|0,0,160);f[e+400>>2]=1;f[e+360>>2]=1;t=e+400|0;y=e+360|0;m=255;b:{while(1){c=m;if(!(g[m+(e+1920|0)|0]|g[(e+2176|0)+m|0])){m=c+ -1|0;if(c){continue}break b}break}if((c|0)<0){break b}x=e+440|0;m=e+2512|0;s=e+2472|0;b=e+2552|0;while(1){_(e+2432|0,e+320|0);a=g[(e+2176|0)+c|0];if(a){M(e+320|0,e+2432|0,b);M(y,s,m);M(t,m,b);M(x,e+2432|0,s);j=f[e+360>>2];h=f[e+320>>2];i=(j-h|0)+134217690|0;f[e+2784>>2]=i&67108863;k=i>>>26|0;i=f[e+364>>2];n=f[e+324>>2];o=((k+i|0)-n|0)+67108862|0;f[e+2788>>2]=o&33554431;v=f[e+368>>2];p=f[e+328>>2];o=(v-p+(o>>>25)|0)+134217726|0;f[e+2792>>2]=o&67108863;q=f[e+372>>2];r=f[e+332>>2];o=(q-r+(o>>>26)|0)+67108862|0;f[e+2796>>2]=o&33554431;k=f[e+376>>2];u=f[e+336>>2];f[e+2800>>2]=(k-u+(o>>>25)|0)+134217726;o=f[e+380>>2];w=f[e+340>>2];f[e+2804>>2]=(o-w|0)+67108862;f[e+2736>>2]=h+j;f[e+2740>>2]=i+n;f[e+2744>>2]=p+v;f[e+2748>>2]=q+r;f[e+2752>>2]=k+u;f[e+2756>>2]=o+w;j=f[e+344>>2];h=f[e+384>>2];f[e+2760>>2]=j+h;i=f[e+348>>2];n=f[e+388>>2];f[e+2764>>2]=i+n;o=f[e+352>>2];v=f[e+392>>2];f[e+2768>>2]=o+v;p=f[e+356>>2];q=f[e+396>>2];f[e+2772>>2]=p+q;f[e+2808>>2]=(h-j|0)+134217726;f[e+2812>>2]=(n-i|0)+67108862;f[e+2816>>2]=(v-o|0)+134217726;f[e+2820>>2]=(q-p|0)+67108862;i=a<<24>>24;j=i>>31;h=a>>>7|0;a=(e+640|0)+l((j^i+j)>>>1|0,160)|0;M(e+2784|0,e+2784|0,l(h,40)+a|0);j=h^1;M(e+2432|0,e+2736|0,a+l(j,40)|0);i=f[e+2784>>2];n=f[e+2432>>2];f[e+2472>>2]=i+n;o=f[e+2788>>2];v=f[e+2436>>2];f[e+2476>>2]=o+v;p=f[e+2792>>2];q=f[e+2440>>2];f[e+2480>>2]=p+q;r=f[e+2796>>2];k=f[e+2444>>2];f[e+2484>>2]=r+k;u=f[e+2800>>2];w=f[e+2448>>2];f[e+2488>>2]=u+w;z=f[e+2804>>2];B=f[e+2452>>2];f[e+2492>>2]=z+B;f[e+2452>>2]=(B-z|0)+67108862;i=(n-i|0)+134217690|0;f[e+2432>>2]=i&67108863;n=f[e+2808>>2];z=f[e+2456>>2];f[e+2496>>2]=n+z;B=f[e+2812>>2];C=f[e+2460>>2];f[e+2500>>2]=B+C;D=f[e+2816>>2];E=f[e+2464>>2];f[e+2504>>2]=D+E;F=f[e+2820>>2];G=f[e+2468>>2];f[e+2508>>2]=F+G;f[e+2460>>2]=(C-B|0)+67108862;f[e+2464>>2]=(E-D|0)+134217726;f[e+2468>>2]=(G-F|0)+67108862;f[e+2456>>2]=(z-n|0)+134217726;i=((v+(i>>>26|0)|0)-o|0)+67108862|0;f[e+2436>>2]=i&33554431;i=(q-p+(i>>>25)|0)+134217726|0;f[e+2440>>2]=i&67108863;i=(k-r+(i>>>26)|0)+67108862|0;f[e+2444>>2]=i&33554431;f[e+2448>>2]=(w-u+(i>>>25)|0)+134217726;M(e+2688|0,x,a+120|0);M(b,t,a+80|0);a=f[e+2552>>2];i=(a>>>25&63)+(f[e+2556>>2]<<1)|0;n=i&33554431;f[e+2556>>2]=n;i=(f[e+2560>>2]<<1)+(i>>>25|0)|0;o=i&67108863;f[e+2560>>2]=o;i=(f[e+2564>>2]<<1)+(i>>>26|0)|0;v=i&33554431;f[e+2564>>2]=v;i=(f[e+2568>>2]<<1)+(i>>>25|0)|0;p=i&67108863;f[e+2568>>2]=p;i=(f[e+2572>>2]<<1)+(i>>>26|0)|0;q=i&33554431;f[e+2572>>2]=q;i=(f[e+2576>>2]<<1)+(i>>>25|0)|0;r=i&67108863;f[e+2576>>2]=r;i=(f[e+2580>>2]<<1)+(i>>>26|0)|0;k=i&33554431;f[e+2580>>2]=k;i=(f[e+2584>>2]<<1)+(i>>>25|0)|0;u=i&67108863;f[e+2584>>2]=u;i=(f[e+2588>>2]<<1)+(i>>>26|0)|0;w=i&33554431;f[e+2588>>2]=w;a=l(i>>>25|0,19)+(a<<1&67108862)|0;f[e+2552>>2]=a;f[e+2548>>2]=w;f[e+2544>>2]=u;f[e+2540>>2]=k;f[e+2536>>2]=r;f[e+2532>>2]=q;f[e+2528>>2]=p;f[e+2524>>2]=v;f[e+2520>>2]=o;f[e+2516>>2]=n;f[e+2512>>2]=a;a=(e+2432|0)+l(h|2,40)|0;h=f[e+2688>>2];f[a>>2]=h+f[a>>2];i=f[e+2692>>2];f[a+4>>2]=i+f[a+4>>2];n=f[e+2696>>2];f[a+8>>2]=n+f[a+8>>2];o=f[e+2700>>2];f[a+12>>2]=o+f[a+12>>2];v=f[e+2704>>2];f[a+16>>2]=v+f[a+16>>2];p=f[e+2708>>2];f[a+20>>2]=p+f[a+20>>2];q=f[e+2712>>2];f[a+24>>2]=q+f[a+24>>2];r=f[e+2716>>2];f[a+28>>2]=r+f[a+28>>2];k=f[e+2720>>2];f[a+32>>2]=k+f[a+32>>2];u=f[e+2724>>2];f[a+36>>2]=u+f[a+36>>2];a=(e+2432|0)+l(j|2,40)|0;f[a+20>>2]=(f[a+20>>2]-p|0)+67108862;f[a+24>>2]=(f[a+24>>2]-q|0)+134217726;f[a+28>>2]=(f[a+28>>2]-r|0)+67108862;f[a+32>>2]=(f[a+32>>2]-k|0)+134217726;f[a+36>>2]=(f[a+36>>2]-u|0)+67108862;j=(f[a>>2]-h|0)+134217690|0;f[a>>2]=j&67108863;j=(f[a+4>>2]-i+(j>>>26)|0)+67108862|0;f[a+4>>2]=j&33554431;j=(f[a+8>>2]-n+(j>>>25)|0)+134217726|0;f[a+8>>2]=j&67108863;j=(f[a+12>>2]-o+(j>>>26)|0)+67108862|0;f[a+12>>2]=j&33554431;f[a+16>>2]=(f[a+16>>2]-v+(j>>>25)|0)+134217726}a=g[(e+1920|0)+c|0];if(a){M(e+320|0,e+2432|0,b);M(y,s,m);M(t,m,b);M(x,e+2432|0,s);j=f[e+360>>2];h=f[e+320>>2];i=(j-h|0)+134217690|0;f[e+2784>>2]=i&67108863;k=i>>>26|0;i=f[e+364>>2];n=f[e+324>>2];o=((k+i|0)-n|0)+67108862|0;f[e+2788>>2]=o&33554431;v=f[e+368>>2];p=f[e+328>>2];o=(v-p+(o>>>25)|0)+134217726|0;f[e+2792>>2]=o&67108863;q=f[e+372>>2];r=f[e+332>>2];o=(q-r+(o>>>26)|0)+67108862|0;f[e+2796>>2]=o&33554431;k=f[e+376>>2];u=f[e+336>>2];f[e+2800>>2]=(k-u+(o>>>25)|0)+134217726;o=f[e+380>>2];w=f[e+340>>2];f[e+2804>>2]=(o-w|0)+67108862;f[e+2736>>2]=h+j;f[e+2740>>2]=i+n;f[e+2744>>2]=p+v;f[e+2748>>2]=q+r;f[e+2752>>2]=k+u;f[e+2756>>2]=o+w;j=f[e+344>>2];h=f[e+384>>2];f[e+2760>>2]=j+h;i=f[e+348>>2];n=f[e+388>>2];f[e+2764>>2]=i+n;o=f[e+352>>2];v=f[e+392>>2];f[e+2768>>2]=o+v;p=f[e+356>>2];q=f[e+396>>2];f[e+2772>>2]=p+q;f[e+2808>>2]=(h-j|0)+134217726;f[e+2812>>2]=(n-i|0)+67108862;f[e+2816>>2]=(v-o|0)+134217726;f[e+2820>>2]=(q-p|0)+67108862;h=a<<24>>24;j=h>>31;j=l((j^h+j)>>>1|0,120);h=j+25776|0;a=a>>>7|0;M(e+2784|0,e+2784|0,h+l(a,40)|0);i=a^1;M(e+2432|0,e+2736|0,h+l(i,40)|0);h=f[e+2784>>2];n=f[e+2432>>2];f[e+2472>>2]=h+n;o=f[e+2788>>2];v=f[e+2436>>2];f[e+2476>>2]=o+v;p=f[e+2792>>2];q=f[e+2440>>2];f[e+2480>>2]=p+q;r=f[e+2796>>2];k=f[e+2444>>2];f[e+2484>>2]=r+k;u=f[e+2800>>2];w=f[e+2448>>2];f[e+2488>>2]=u+w;z=f[e+2804>>2];B=f[e+2452>>2];f[e+2492>>2]=z+B;f[e+2452>>2]=(B-z|0)+67108862;h=(n-h|0)+134217690|0;f[e+2432>>2]=h&67108863;n=f[e+2808>>2];z=f[e+2456>>2];f[e+2496>>2]=n+z;B=f[e+2812>>2];C=f[e+2460>>2];f[e+2500>>2]=B+C;D=f[e+2816>>2];E=f[e+2464>>2];f[e+2504>>2]=D+E;F=f[e+2820>>2];G=f[e+2468>>2];f[e+2508>>2]=F+G;f[e+2460>>2]=(C-B|0)+67108862;f[e+2464>>2]=(E-D|0)+134217726;f[e+2468>>2]=(G-F|0)+67108862;f[e+2456>>2]=(z-n|0)+134217726;h=((v+(h>>>26|0)|0)-o|0)+67108862|0;f[e+2436>>2]=h&33554431;h=(q-p+(h>>>25)|0)+134217726|0;f[e+2440>>2]=h&67108863;h=(k-r+(h>>>26)|0)+67108862|0;f[e+2444>>2]=h&33554431;f[e+2448>>2]=(w-u+(h>>>25)|0)+134217726;M(e+2688|0,x,j+25856|0);j=f[e+400>>2];h=(j>>>25&63)+(f[e+404>>2]<<1)|0;n=h&33554431;f[e+2556>>2]=n;h=(f[e+408>>2]<<1)+(h>>>25|0)|0;o=h&67108863;f[e+2560>>2]=o;h=(f[e+412>>2]<<1)+(h>>>26|0)|0;v=h&33554431;f[e+2564>>2]=v;h=(f[e+416>>2]<<1)+(h>>>25|0)|0;p=h&67108863;f[e+2568>>2]=p;h=(f[e+420>>2]<<1)+(h>>>26|0)|0;q=h&33554431;f[e+2572>>2]=q;h=(f[e+424>>2]<<1)+(h>>>25|0)|0;r=h&67108863;f[e+2576>>2]=r;h=(f[e+428>>2]<<1)+(h>>>26|0)|0;k=h&33554431;f[e+2580>>2]=k;h=(f[e+432>>2]<<1)+(h>>>25|0)|0;u=h&67108863;f[e+2584>>2]=u;h=(f[e+436>>2]<<1)+(h>>>26|0)|0;w=h&33554431;f[e+2588>>2]=w;f[e+2516>>2]=n;f[e+2520>>2]=o;f[e+2524>>2]=v;f[e+2528>>2]=p;f[e+2532>>2]=q;f[e+2536>>2]=r;f[e+2540>>2]=k;f[e+2544>>2]=u;f[e+2548>>2]=w;j=l(h>>>25|0,19)+(j<<1&67108862)|0;f[e+2512>>2]=j;f[e+2552>>2]=j;a=(e+2432|0)+l(a|2,40)|0;j=f[e+2688>>2];f[a>>2]=j+f[a>>2];h=f[e+2692>>2];f[a+4>>2]=h+f[a+4>>2];n=f[e+2696>>2];f[a+8>>2]=n+f[a+8>>2];o=f[e+2700>>2];f[a+12>>2]=o+f[a+12>>2];v=f[e+2704>>2];f[a+16>>2]=v+f[a+16>>2];p=f[e+2708>>2];f[a+20>>2]=p+f[a+20>>2];q=f[e+2712>>2];f[a+24>>2]=q+f[a+24>>2];r=f[e+2716>>2];f[a+28>>2]=r+f[a+28>>2];k=f[e+2720>>2];f[a+32>>2]=k+f[a+32>>2];u=f[e+2724>>2];f[a+36>>2]=u+f[a+36>>2];a=(e+2432|0)+l(i|2,40)|0;f[a+20>>2]=(f[a+20>>2]-p|0)+67108862;f[a+24>>2]=(f[a+24>>2]-q|0)+134217726;f[a+28>>2]=(f[a+28>>2]-r|0)+67108862;f[a+32>>2]=(f[a+32>>2]-k|0)+134217726;f[a+36>>2]=(f[a+36>>2]-u|0)+67108862;j=(f[a>>2]-j|0)+134217690|0;f[a>>2]=j&67108863;j=(f[a+4>>2]-h+(j>>>26)|0)+67108862|0;f[a+4>>2]=j&33554431;j=(f[a+8>>2]-n+(j>>>25)|0)+134217726|0;f[a+8>>2]=j&67108863;j=(f[a+12>>2]-o+(j>>>26)|0)+67108862|0;f[a+12>>2]=j&33554431;f[a+16>>2]=(f[a+16>>2]-v+(j>>>25)|0)+134217726}M(e+320|0,e+2432|0,b);M(y,s,m);M(t,m,b);a=(c|0)>0;c=c+ -1|0;if(a){continue}break}}da(e+640|0,e+320|0);m=ha(d,e+640|0)?0:-1}A=e+2832|0;return m}function xa(a){a=a|0;var b=0,c=0,d=0,e=0,h=0,j=0,k=0,l=0,m=0,n=0,o=0;o=A-16|0;A=o;a:{b:{c:{d:{e:{f:{g:{h:{i:{j:{k:{if(a>>>0<=244){h=f[7765];j=a>>>0<11?16:a+11&-8;a=j>>>3|0;b=h>>>a|0;if(b&3){c=a+((b^-1)&1)|0;e=c<<3;b=f[e+31108>>2];a=b+8|0;d=f[b+8>>2];e=e+31100|0;l:{if((d|0)==(e|0)){f[7765]=ib(-2,c)&h;break l}f[d+12>>2]=e;f[e+8>>2]=d}c=c<<3;f[b+4>>2]=c|3;b=b+c|0;f[b+4>>2]=f[b+4>>2]|1;break a}l=f[7767];if(j>>>0<=l>>>0){break k}if(b){c=2<<a;a=(0-c|c)&b<<a;a=(0-a&a)+ -1|0;b=a>>>12&16;c=b;a=a>>>b|0;b=a>>>5&8;c=c|b;a=a>>>b|0;b=a>>>2&4;c=c|b;a=a>>>b|0;b=a>>>1&2;c=c|b;a=a>>>b|0;b=a>>>1&1;c=(c|b)+(a>>>b|0)|0;d=c<<3;b=f[d+31108>>2];a=f[b+8>>2];d=d+31100|0;m:{if((a|0)==(d|0)){h=ib(-2,c)&h;f[7765]=h;break m}f[a+12>>2]=d;f[d+8>>2]=a}a=b+8|0;f[b+4>>2]=j|3;k=b+j|0;c=c<<3;e=c-j|0;f[k+4>>2]=e|1;f[b+c>>2]=e;if(l){c=l>>>3|0;b=(c<<3)+31100|0;d=f[7770];c=1<<c;n:{if(!(c&h)){f[7765]=c|h;c=b;break n}c=f[b+8>>2]}f[b+8>>2]=d;f[c+12>>2]=d;f[d+12>>2]=b;f[d+8>>2]=c}f[7770]=k;f[7767]=e;break a}n=f[7766];if(!n){break k}a=(n&0-n)+ -1|0;b=a>>>12&16;c=b;a=a>>>b|0;b=a>>>5&8;c=c|b;a=a>>>b|0;b=a>>>2&4;c=c|b;a=a>>>b|0;b=a>>>1&2;c=c|b;a=a>>>b|0;b=a>>>1&1;b=f[((c|b)+(a>>>b|0)<<2)+31364>>2];d=(f[b+4>>2]&-8)-j|0;c=b;while(1){o:{a=f[c+16>>2];if(!a){a=f[c+20>>2];if(!a){break o}}e=(f[a+4>>2]&-8)-j|0;c=e>>>0<d>>>0;d=c?e:d;b=c?a:b;c=a;continue}break}m=f[b+24>>2];e=f[b+12>>2];if((e|0)!=(b|0)){a=f[b+8>>2];f[a+12>>2]=e;f[e+8>>2]=a;break b}c=b+20|0;a=f[c>>2];if(!a){a=f[b+16>>2];if(!a){break j}c=b+16|0}while(1){k=c;e=a;c=a+20|0;a=f[c>>2];if(a){continue}c=e+16|0;a=f[e+16>>2];if(a){continue}break}f[k>>2]=0;break b}j=-1;if(a>>>0>4294967231){break k}b=a+11|0;j=b&-8;l=f[7766];if(!l){break k}c=0-j|0;b=b>>>8|0;h=0;p:{if(!b){break p}h=31;if(j>>>0>16777215){break p}d=b+1048320>>>16&8;b=b<<d;a=b+520192>>>16&4;h=b<<a;b=h+245760>>>16&2;a=(h<<b>>>15|0)-(b|(a|d))|0;h=(a<<1|j>>>a+21&1)+28|0}d=f[(h<<2)+31364>>2];q:{r:{s:{if(!d){a=0;break s}b=j<<((h|0)==31?0:25-(h>>>1|0)|0);a=0;while(1){t:{k=(f[d+4>>2]&-8)-j|0;if(k>>>0>=c>>>0){break t}e=d;c=k;if(c){break t}c=0;a=d;break r}k=f[d+20>>2];d=f[((b>>>29&4)+d|0)+16>>2];a=k?(k|0)==(d|0)?a:k:a;b=b<<((d|0)!=0);if(d){continue}break}}if(!(a|e)){a=2<<h;a=(0-a|a)&l;if(!a){break k}a=(a&0-a)+ -1|0;b=a>>>12&16;d=b;a=a>>>b|0;b=a>>>5&8;d=d|b;a=a>>>b|0;b=a>>>2&4;d=d|b;a=a>>>b|0;b=a>>>1&2;d=d|b;a=a>>>b|0;b=a>>>1&1;a=f[((d|b)+(a>>>b|0)<<2)+31364>>2]}if(!a){break q}}while(1){d=(f[a+4>>2]&-8)-j|0;b=d>>>0<c>>>0;c=b?d:c;e=b?a:e;b=f[a+16>>2];if(b){a=b}else{a=f[a+20>>2]}if(a){continue}break}}if(!e|c>>>0>=f[7767]-j>>>0){break k}k=f[e+24>>2];b=f[e+12>>2];if((e|0)!=(b|0)){a=f[e+8>>2];f[a+12>>2]=b;f[b+8>>2]=a;break c}d=e+20|0;a=f[d>>2];if(!a){a=f[e+16>>2];if(!a){break i}d=e+16|0}while(1){h=d;b=a;d=a+20|0;a=f[d>>2];if(a){continue}d=b+16|0;a=f[b+16>>2];if(a){continue}break}f[h>>2]=0;break c}b=f[7767];if(b>>>0>=j>>>0){a=f[7770];c=b-j|0;u:{if(c>>>0>=16){f[7767]=c;d=a+j|0;f[7770]=d;f[d+4>>2]=c|1;f[a+b>>2]=c;f[a+4>>2]=j|3;break u}f[7770]=0;f[7767]=0;f[a+4>>2]=b|3;b=a+b|0;f[b+4>>2]=f[b+4>>2]|1}a=a+8|0;break a}d=f[7768];if(d>>>0>j>>>0){b=d-j|0;f[7768]=b;a=f[7771];c=a+j|0;f[7771]=c;f[c+4>>2]=b|1;f[a+4>>2]=j|3;a=a+8|0;break a}a=0;e=j+47|0;c=e;if(f[7883]){b=f[7885]}else{f[7886]=-1;f[7887]=-1;f[7884]=4096;f[7885]=4096;f[7883]=o+12&-16^1431655768;f[7888]=0;f[7876]=0;b=4096}h=c+b|0;k=0-b|0;c=h&k;if(c>>>0<=j>>>0){break a}b=f[7875];if(b){l=f[7873];m=l+c|0;if(m>>>0<=l>>>0|m>>>0>b>>>0){break a}}if(g[31504]&4){break f}v:{w:{b=f[7771];if(b){a=31508;while(1){l=f[a>>2];if(l+f[a+4>>2]>>>0>b>>>0?l>>>0<=b>>>0:0){break w}a=f[a+8>>2];if(a){continue}break}}b=W(0);if((b|0)==-1){break g}h=c;a=f[7884];d=a+ -1|0;if(d&b){h=(c-b|0)+(b+d&0-a)|0}if(h>>>0<=j>>>0|h>>>0>2147483646){break g}a=f[7875];if(a){d=f[7873];k=d+h|0;if(k>>>0<=d>>>0|k>>>0>a>>>0){break g}}a=W(h);if((b|0)!=(a|0)){break v}break e}h=k&h-d;if(h>>>0>2147483646){break g}b=W(h);if((b|0)==(f[a>>2]+f[a+4>>2]|0)){break h}a=b}if(!((a|0)==-1|j+48>>>0<=h>>>0)){b=f[7885];b=b+(e-h|0)&0-b;if(b>>>0>2147483646){b=a;break e}if((W(b)|0)!=-1){h=b+h|0;b=a;break e}W(0-h|0);break g}b=a;if((a|0)!=-1){break e}break g}e=0;break b}b=0;break c}if((b|0)!=-1){break e}}f[7876]=f[7876]|4}if(c>>>0>2147483646){break d}b=W(c);a=W(0);if(b>>>0>=a>>>0|(b|0)==-1|(a|0)==-1){break d}h=a-b|0;if(h>>>0<=j+40>>>0){break d}}a=f[7873]+h|0;f[7873]=a;if(a>>>0>i[7874]){f[7874]=a}x:{y:{z:{c=f[7771];if(c){a=31508;while(1){d=f[a>>2];e=f[a+4>>2];if((d+e|0)==(b|0)){break z}a=f[a+8>>2];if(a){continue}break}break y}a=f[7769];if(!(b>>>0>=a>>>0?a:0)){f[7769]=b}a=0;f[7878]=h;f[7877]=b;f[7773]=-1;f[7774]=f[7883];f[7880]=0;while(1){c=a<<3;d=c+31100|0;f[c+31108>>2]=d;f[c+31112>>2]=d;a=a+1|0;if((a|0)!=32){continue}break}a=h+ -40|0;c=b+8&7?-8-b&7:0;d=a-c|0;f[7768]=d;c=b+c|0;f[7771]=c;f[c+4>>2]=d|1;f[(a+b|0)+4>>2]=40;f[7772]=f[7887];break x}if(g[a+12|0]&8|b>>>0<=c>>>0|d>>>0>c>>>0){break y}f[a+4>>2]=e+h;a=c+8&7?-8-c&7:0;b=a+c|0;f[7771]=b;d=f[7768]+h|0;a=d-a|0;f[7768]=a;f[b+4>>2]=a|1;f[(c+d|0)+4>>2]=40;f[7772]=f[7887];break x}e=f[7769];if(b>>>0<e>>>0){f[7769]=b;e=0}d=b+h|0;a=31508;A:{B:{C:{D:{E:{F:{while(1){if((d|0)!=f[a>>2]){a=f[a+8>>2];if(a){continue}break F}break}if(!(g[a+12|0]&8)){break E}}a=31508;while(1){d=f[a>>2];if(d>>>0<=c>>>0){e=d+f[a+4>>2]|0;if(e>>>0>c>>>0){break D}}a=f[a+8>>2];continue}}f[a>>2]=b;f[a+4>>2]=f[a+4>>2]+h;m=(b+8&7?-8-b&7:0)+b|0;f[m+4>>2]=j|3;b=d+(d+8&7?-8-d&7:0)|0;a=(b-m|0)-j|0;k=j+m|0;if((b|0)==(c|0)){f[7771]=k;a=f[7768]+a|0;f[7768]=a;f[k+4>>2]=a|1;break B}if(f[7770]==(b|0)){f[7770]=k;a=f[7767]+a|0;f[7767]=a;f[k+4>>2]=a|1;f[a+k>>2]=a;break B}c=f[b+4>>2];if((c&3)==1){n=c&-8;G:{if(c>>>0<=255){e=c>>>3|0;c=f[b+8>>2];d=f[b+12>>2];if((d|0)==(c|0)){f[7765]=f[7765]&ib(-2,e);break G}f[c+12>>2]=d;f[d+8>>2]=c;break G}l=f[b+24>>2];h=f[b+12>>2];H:{if((h|0)!=(b|0)){c=f[b+8>>2];f[c+12>>2]=h;f[h+8>>2]=c;break H}I:{d=b+20|0;j=f[d>>2];if(j){break I}d=b+16|0;j=f[d>>2];if(j){break I}h=0;break H}while(1){c=d;h=j;d=j+20|0;j=f[d>>2];if(j){continue}d=h+16|0;j=f[h+16>>2];if(j){continue}break}f[c>>2]=0}if(!l){break G}c=f[b+28>>2];d=(c<<2)+31364|0;J:{if(f[d>>2]==(b|0)){f[d>>2]=h;if(h){break J}f[7766]=f[7766]&ib(-2,c);break G}f[l+(f[l+16>>2]==(b|0)?16:20)>>2]=h;if(!h){break G}}f[h+24>>2]=l;c=f[b+16>>2];if(c){f[h+16>>2]=c;f[c+24>>2]=h}c=f[b+20>>2];if(!c){break G}f[h+20>>2]=c;f[c+24>>2]=h}b=b+n|0;a=a+n|0}f[b+4>>2]=f[b+4>>2]&-2;f[k+4>>2]=a|1;f[a+k>>2]=a;if(a>>>0<=255){b=a>>>3|0;a=(b<<3)+31100|0;c=f[7765];b=1<<b;K:{if(!(c&b)){f[7765]=b|c;b=a;break K}b=f[a+8>>2]}f[a+8>>2]=k;f[b+12>>2]=k;f[k+12>>2]=a;f[k+8>>2]=b;break B}c=k;d=a>>>8|0;b=0;L:{if(!d){break L}b=31;if(a>>>0>16777215){break L}e=d+1048320>>>16&8;d=d<<e;b=d+520192>>>16&4;j=d<<b;d=j+245760>>>16&2;b=(j<<d>>>15|0)-(d|(b|e))|0;b=(b<<1|a>>>b+21&1)+28|0}f[c+28>>2]=b;f[k+16>>2]=0;f[k+20>>2]=0;c=(b<<2)+31364|0;d=f[7766];e=1<<b;M:{if(!(d&e)){f[7766]=d|e;f[c>>2]=k;break M}d=a<<((b|0)==31?0:25-(b>>>1|0)|0);b=f[c>>2];while(1){c=b;if((f[b+4>>2]&-8)==(a|0)){break C}b=d>>>29|0;d=d<<1;e=(b&4)+c|0;b=f[e+16>>2];if(b){continue}break}f[e+16>>2]=k}f[k+24>>2]=c;f[k+12>>2]=k;f[k+8>>2]=k;break B}a=h+ -40|0;d=b+8&7?-8-b&7:0;k=a-d|0;f[7768]=k;d=b+d|0;f[7771]=d;f[d+4>>2]=k|1;f[(a+b|0)+4>>2]=40;f[7772]=f[7887];a=(e+(e+ -39&7?39-e&7:0)|0)+ -47|0;d=a>>>0<c+16>>>0?c:a;f[d+4>>2]=27;a=f[7880];f[d+16>>2]=f[7879];f[d+20>>2]=a;a=f[7878];f[d+8>>2]=f[7877];f[d+12>>2]=a;f[7879]=d+8;f[7878]=h;f[7877]=b;f[7880]=0;a=d+24|0;while(1){f[a+4>>2]=7;b=a+8|0;a=a+4|0;if(e>>>0>b>>>0){continue}break}if((c|0)==(d|0)){break x}f[d+4>>2]=f[d+4>>2]&-2;e=d-c|0;f[c+4>>2]=e|1;f[d>>2]=e;if(e>>>0<=255){b=e>>>3|0;a=(b<<3)+31100|0;d=f[7765];b=1<<b;N:{if(!(d&b)){f[7765]=b|d;b=a;break N}b=f[a+8>>2]}f[a+8>>2]=c;f[b+12>>2]=c;f[c+12>>2]=a;f[c+8>>2]=b;break x}f[c+16>>2]=0;f[c+20>>2]=0;b=c;d=e>>>8|0;a=0;O:{if(!d){break O}a=31;if(e>>>0>16777215){break O}h=d+1048320>>>16&8;d=d<<h;a=d+520192>>>16&4;k=d<<a;d=k+245760>>>16&2;a=(k<<d>>>15|0)-(d|(a|h))|0;a=(a<<1|e>>>a+21&1)+28|0}f[b+28>>2]=a;b=(a<<2)+31364|0;d=f[7766];h=1<<a;P:{if(!(d&h)){f[7766]=d|h;f[b>>2]=c;f[c+24>>2]=b;break P}a=e<<((a|0)==31?0:25-(a>>>1|0)|0);b=f[b>>2];while(1){d=b;if((e|0)==(f[b+4>>2]&-8)){break A}b=a>>>29|0;a=a<<1;h=d+(b&4)|0;b=f[h+16>>2];if(b){continue}break}f[h+16>>2]=c;f[c+24>>2]=d}f[c+12>>2]=c;f[c+8>>2]=c;break x}a=f[c+8>>2];f[a+12>>2]=k;f[c+8>>2]=k;f[k+24>>2]=0;f[k+12>>2]=c;f[k+8>>2]=a}a=m+8|0;break a}a=f[d+8>>2];f[a+12>>2]=c;f[d+8>>2]=c;f[c+24>>2]=0;f[c+12>>2]=d;f[c+8>>2]=a}a=f[7768];if(a>>>0<=j>>>0){break d}b=a-j|0;f[7768]=b;a=f[7771];c=a+j|0;f[7771]=c;f[c+4>>2]=b|1;f[a+4>>2]=j|3;a=a+8|0;break a}f[7764]=48;a=0;break a}Q:{if(!k){break Q}a=f[e+28>>2];d=(a<<2)+31364|0;R:{if(f[d>>2]==(e|0)){f[d>>2]=b;if(b){break R}l=ib(-2,a)&l;f[7766]=l;break Q}f[k+(f[k+16>>2]==(e|0)?16:20)>>2]=b;if(!b){break Q}}f[b+24>>2]=k;a=f[e+16>>2];if(a){f[b+16>>2]=a;f[a+24>>2]=b}a=f[e+20>>2];if(!a){break Q}f[b+20>>2]=a;f[a+24>>2]=b}S:{if(c>>>0<=15){a=c+j|0;f[e+4>>2]=a|3;a=a+e|0;f[a+4>>2]=f[a+4>>2]|1;break S}f[e+4>>2]=j|3;d=e+j|0;f[d+4>>2]=c|1;f[c+d>>2]=c;if(c>>>0<=255){b=c>>>3|0;a=(b<<3)+31100|0;c=f[7765];b=1<<b;T:{if(!(c&b)){f[7765]=b|c;b=a;break T}b=f[a+8>>2]}f[a+8>>2]=d;f[b+12>>2]=d;f[d+12>>2]=a;f[d+8>>2]=b;break S}b=d;j=c>>>8|0;a=0;U:{if(!j){break U}a=31;if(c>>>0>16777215){break U}h=j+1048320>>>16&8;j=j<<h;a=j+520192>>>16&4;k=j<<a;j=k+245760>>>16&2;a=(k<<j>>>15|0)-(j|(a|h))|0;a=(a<<1|c>>>a+21&1)+28|0}f[b+28>>2]=a;f[d+16>>2]=0;f[d+20>>2]=0;b=(a<<2)+31364|0;V:{j=1<<a;W:{if(!(j&l)){f[7766]=j|l;f[b>>2]=d;break W}a=c<<((a|0)==31?0:25-(a>>>1|0)|0);j=f[b>>2];while(1){b=j;if((f[b+4>>2]&-8)==(c|0)){break V}j=a>>>29|0;a=a<<1;h=(j&4)+b|0;j=f[h+16>>2];if(j){continue}break}f[h+16>>2]=d}f[d+24>>2]=b;f[d+12>>2]=d;f[d+8>>2]=d;break S}a=f[b+8>>2];f[a+12>>2]=d;f[b+8>>2]=d;f[d+24>>2]=0;f[d+12>>2]=b;f[d+8>>2]=a}a=e+8|0;break a}X:{if(!m){break X}a=f[b+28>>2];c=(a<<2)+31364|0;Y:{if(f[c>>2]==(b|0)){f[c>>2]=e;if(e){break Y}f[7766]=ib(-2,a)&n;break X}f[m+(f[m+16>>2]==(b|0)?16:20)>>2]=e;if(!e){break X}}f[e+24>>2]=m;a=f[b+16>>2];if(a){f[e+16>>2]=a;f[a+24>>2]=e}a=f[b+20>>2];if(!a){break X}f[e+20>>2]=a;f[a+24>>2]=e}Z:{if(d>>>0<=15){a=d+j|0;f[b+4>>2]=a|3;a=a+b|0;f[a+4>>2]=f[a+4>>2]|1;break Z}f[b+4>>2]=j|3;j=b+j|0;f[j+4>>2]=d|1;f[d+j>>2]=d;if(l){c=l>>>3|0;a=(c<<3)+31100|0;e=f[7770];c=1<<c;_:{if(!(c&h)){f[7765]=c|h;c=a;break _}c=f[a+8>>2]}f[a+8>>2]=e;f[c+12>>2]=e;f[e+12>>2]=a;f[e+8>>2]=c}f[7770]=j;f[7767]=d}a=b+8|0}A=o+16|0;return a|0}function ga(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0,R=0,S=0,T=0,U=0;C=A-640|0;A=C;g=f[b>>2];n=g<<24|g<<8&16711680;e=f[b+4>>2];c=e<<24|g>>>8;t=c&65280;c=e<<8|g>>>24;n=c&255|t|n;c=e;p=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|d;c=n;n=c;h=C;f[h>>2]=p;f[h+4>>2]=c;d=f[b+8>>2];g=d<<24|d<<8&16711680;e=f[b+12>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+8>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+12>>2]=g;d=f[b+16>>2];g=d<<24|d<<8&16711680;e=f[b+20>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+16>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+20>>2]=g;d=f[b+24>>2];g=d<<24|d<<8&16711680;e=f[b+28>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+24>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+28>>2]=g;d=f[b+32>>2];g=d<<24|d<<8&16711680;e=f[b+36>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+32>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+36>>2]=g;d=f[b+40>>2];g=d<<24|d<<8&16711680;e=f[b+44>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+40>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+44>>2]=g;d=f[b+48>>2];g=d<<24|d<<8&16711680;e=f[b+52>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+48>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+52>>2]=g;d=f[b+56>>2];g=d<<24|d<<8&16711680;e=f[b+60>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+56>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+60>>2]=g;d=f[b+64>>2];g=d<<24|d<<8&16711680;e=f[b+68>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+64>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+68>>2]=g;d=f[b+72>>2];g=d<<24|d<<8&16711680;e=f[b+76>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+72>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+76>>2]=g;d=f[b+80>>2];g=d<<24|d<<8&16711680;e=f[b+84>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+80>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+84>>2]=g;d=f[b+88>>2];g=d<<24|d<<8&16711680;e=f[b+92>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+88>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+92>>2]=g;d=f[b+96>>2];g=d<<24|d<<8&16711680;e=f[b+100>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+96>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+100>>2]=g;d=f[b+104>>2];g=d<<24|d<<8&16711680;e=f[b+108>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+104>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+108>>2]=g;d=f[b+112>>2];g=d<<24|d<<8&16711680;e=f[b+116>>2];c=e<<24|d>>>8;i=c&65280;c=e<<8|d>>>24;g=c&255|i|g;c=e;f[h+112>>2]=((c&255)<<24|d>>>8)&-16777216|((c&16777215)<<8|d>>>24)&16711680|(c>>>8&65280|c>>>24)|j;f[h+116>>2]=g;e=f[b+124>>2];g=f[b+120>>2];j=g<<24|g<<8&16711680;c=e<<24|g>>>8;b=0;t=c&65280;c=e<<8|g>>>24;d=c&255|t|j;f[h+120>>2]=((e&255)<<24|g>>>8)&-16777216|((e&16777215)<<8|g>>>24)&16711680|(e>>>8&65280|e>>>24)|b;f[h+124>>2]=d;b=16;while(1){g=(b<<3)+C|0;e=g;c=e+ -56|0;d=f[c>>2];h=d+p|0;c=f[c+4>>2]+n|0;c=h>>>0<d>>>0?c+1|0:c;n=c;d=e+ -16|0;c=f[d+4>>2];d=f[d>>2];j=jb(d,c,3);t=B;g=h;h=c;c=c>>>6|0;d=jb(d,h,45)^(((h&63)<<26|d>>>6)^j);h=g+d|0;c=(B^(c^t))+n|0;c=h>>>0<d>>>0?c+1|0:c;d=h;h=c;c=e+ -120|0;n=f[c+4>>2];p=f[c>>2];c=jb(p,n,56);j=B;o=d;d=c;g=n;c=g>>>7|0;d=d^((g&127)<<25|p>>>7)^jb(p,g,63);g=o+d|0;c=(B^(c^j))+h|0;f[e>>2]=g;f[e+4>>2]=g>>>0<d>>>0?c+1|0:c;b=b+1|0;if((b|0)!=80){continue}break}b=0;c=a;e=f[c+148>>2];F=f[c+144>>2];p=F;N=e;n=e;j=f[c+156>>2];O=j;G=f[c+152>>2];r=G;h=f[c+204>>2];P=h;H=f[c+200>>2];v=H;g=f[c+196>>2];Q=g;I=f[c+192>>2];m=I;d=f[c+188>>2];R=d;J=f[c+184>>2];l=J;e=f[c+180>>2];S=e;K=f[c+176>>2];s=K;t=f[c+172>>2];T=t;L=f[c+168>>2];q=L;i=f[c+164>>2];U=i;M=f[c+160>>2];x=M;while(1){D=b<<3;c=D+29744|0;o=f[c>>2];k=f[c+4>>2];c=jb(s,e,50);w=B;c=jb(s,e,46)^c;w=B^w;y=o;o=v+(jb(s,e,23)^c)|0;c=h+(B^w)|0;c=o>>>0<v>>>0?c+1|0:c;v=m^s&(l^m);h=v+o|0;c=(g^e&(d^g))+c|0;c=h>>>0<v>>>0?c+1|0:c;v=h;h=y+h|0;c=c+k|0;c=h>>>0<v>>>0?c+1|0:c;v=C+D|0;o=f[v>>2];h=o+h|0;c=f[v+4>>2]+c|0;c=h>>>0<o>>>0?c+1|0:c;o=c;c=jb(p,n,36);k=B;c=jb(p,n,30)^c;w=B^k;u=jb(p,n,25)^c;k=u+(x&(r|p)|r&p)|0;c=(i&(j|n)|j&n)+(B^w)|0;c=k>>>0<u>>>0?c+1|0:c;w=k;k=k+h|0;c=c+o|0;c=k>>>0<w>>>0?c+1|0:c;w=k;v=jb(k,c,36);k=B;y=v;v=c;u=y^jb(w,c,30);y=B^k;k=jb(w,c,25)^u;u=r&(p|w)|p&w;k=k+u|0;c=(j&(c|n)|c&n)+(B^y)|0;y=k;k=k>>>0<u>>>0?c+1|0:c;z=D|8;c=z+29744|0;u=m+f[c>>2]|0;c=g+f[c+4>>2]|0;c=u>>>0<m>>>0?c+1|0:c;g=u;m=z+C|0;u=f[m>>2];g=g+u|0;c=f[m+4>>2]+c|0;c=g>>>0<u>>>0?c+1|0:c;u=g;m=c;c=t+o|0;g=h+q|0;if(g>>>0<h>>>0){c=c+1|0}t=c;c=(d^c&(d^e))+m|0;h=l^(l^s)&g;m=h+u|0;if(m>>>0<h>>>0){c=c+1|0}h=m;m=jb(g,t,50);o=B;m=jb(g,t,46)^m;o=B^o;m=jb(g,t,23)^m;h=m+h|0;c=(B^o)+c|0;c=h>>>0<m>>>0?c+1|0:c;m=c;c=c+k|0;k=h+y|0;if(k>>>0<h>>>0){c=c+1|0}u=k;o=jb(k,c,36);k=B;q=o;o=c;q=q^jb(u,c,30);y=B^k;k=jb(u,c,25)^q;q=p&(u|w)|u&w;k=k+q|0;c=(n&(c|v)|c&v)+(B^y)|0;y=k;k=k>>>0<q>>>0?c+1|0:c;z=D|16;c=z+29744|0;q=l+f[c>>2]|0;c=d+f[c+4>>2]|0;c=q>>>0<l>>>0?c+1|0:c;d=q;l=z+C|0;q=f[l>>2];d=d+q|0;c=f[l+4>>2]+c|0;c=d>>>0<q>>>0?c+1|0:c;q=d;l=c;c=i+m|0;d=h+x|0;if(d>>>0<h>>>0){c=c+1|0}i=c;c=(e^c&(e^t))+l|0;h=s^(g^s)&d;m=h+q|0;if(m>>>0<h>>>0){c=c+1|0}h=m;m=jb(d,i,50);l=B;m=jb(d,i,46)^m;l=B^l;m=jb(d,i,23)^m;h=m+h|0;c=(B^l)+c|0;c=h>>>0<m>>>0?c+1|0:c;m=c;c=c+k|0;k=h+y|0;if(k>>>0<h>>>0){c=c+1|0}y=k;l=jb(k,c,36);q=B;k=c;l=jb(y,c,30)^l;q=B^q;x=w&(u|y)|u&y;l=x+(jb(y,c,25)^l)|0;c=(v&(c|o)|c&o)+(B^q)|0;c=l>>>0<x>>>0?c+1|0:c;x=l;l=c;z=D|24;c=z+29744|0;q=s+f[c>>2]|0;c=e+f[c+4>>2]|0;c=q>>>0<s>>>0?c+1|0:c;e=q;s=z+C|0;q=f[s>>2];e=e+q|0;c=f[s+4>>2]+c|0;s=e;e=e>>>0<q>>>0?c+1|0:c;c=j+m|0;j=h+r|0;if(j>>>0<h>>>0){c=c+1|0}m=c;c=(t^c&(t^i))+e|0;e=g^(d^g)&j;h=e+s|0;if(h>>>0<e>>>0){c=c+1|0}e=h;h=jb(j,m,50);s=B;h=jb(j,m,46)^h;s=B^s;h=jb(j,m,23)^h;e=h+e|0;c=(B^s)+c|0;c=e>>>0<h>>>0?c+1|0:c;h=c;c=c+l|0;s=e+x|0;if(s>>>0<e>>>0){c=c+1|0}z=s;l=jb(s,c,36);r=B;s=c;l=jb(z,c,30)^l;r=B^r;q=u&(y|z)|y&z;l=q+(jb(z,c,25)^l)|0;c=(o&(c|k)|c&k)+(B^r)|0;c=l>>>0<q>>>0?c+1|0:c;q=l;r=c;x=D|32;c=x+29744|0;l=g+f[c>>2]|0;c=t+f[c+4>>2]|0;c=l>>>0<g>>>0?c+1|0:c;g=l;t=x+C|0;l=f[t>>2];g=g+l|0;c=f[t+4>>2]+c|0;t=g;g=g>>>0<l>>>0?c+1|0:c;c=h+n|0;n=e+p|0;if(n>>>0<e>>>0){c=c+1|0}l=c;c=(i^c&(i^m))+g|0;e=d^(d^j)&n;h=e+t|0;if(h>>>0<e>>>0){c=c+1|0}e=h;h=jb(n,l,50);g=B;h=jb(n,l,46)^h;g=B^g;h=jb(n,l,23)^h;e=h+e|0;c=(B^g)+c|0;c=e>>>0<h>>>0?c+1|0:c;h=c;c=c+r|0;t=e+q|0;if(t>>>0<e>>>0){c=c+1|0}q=t;g=jb(q,c,36);r=B;t=c;g=jb(q,c,30)^g;r=B^r;p=y&(q|z)|q&z;g=p+(jb(q,c,25)^g)|0;c=(k&(c|s)|c&s)+(B^r)|0;c=g>>>0<p>>>0?c+1|0:c;p=g;g=c;c=D|40;x=c+C|0;c=c+29744|0;E=f[c>>2];r=f[x>>2]+E|0;c=f[x+4>>2]+f[c+4>>2]|0;c=r>>>0<E>>>0?c+1|0:c;r=d+r|0;c=c+i|0;i=r;d=i>>>0<d>>>0?c+1|0:c;c=h+v|0;h=e+w|0;if(h>>>0<e>>>0){c=c+1|0}v=h;h=c;c=(m^c&(l^m))+d|0;e=j^(j^n)&v;d=e+i|0;if(d>>>0<e>>>0){c=c+1|0}e=d;d=jb(v,h,50);i=B;d=jb(v,h,46)^d;i=B^i;d=jb(v,h,23)^d;e=d+e|0;c=(B^i)+c|0;c=e>>>0<d>>>0?c+1|0:c;d=g;g=c;c=d+c|0;d=e;i=d+p|0;if(i>>>0<d>>>0){c=c+1|0}x=i;d=jb(i,c,36);r=B;i=c;d=jb(x,c,30)^d;r=B^r;p=z&(q|x)|q&x;d=p+(jb(x,c,25)^d)|0;c=(s&(c|t)|c&t)+(B^r)|0;c=d>>>0<p>>>0?c+1|0:c;p=d;d=c;c=D|48;w=c+C|0;c=c+29744|0;E=f[c>>2];r=f[w>>2]+E|0;c=f[w+4>>2]+f[c+4>>2]|0;c=r>>>0<E>>>0?c+1|0:c;r=j+r|0;c=c+m|0;c=r>>>0<j>>>0?c+1|0:c;j=c;c=g+o|0;g=e+u|0;if(g>>>0<e>>>0){c=c+1|0}m=g;g=c;c=(l^c&(h^l))+j|0;e=n^(n^v)&m;j=e+r|0;if(j>>>0<e>>>0){c=c+1|0}e=j;j=jb(m,g,50);o=B;j=jb(m,g,46)^j;o=B^o;j=jb(m,g,23)^j;e=j+e|0;c=(B^o)+c|0;c=e>>>0<j>>>0?c+1|0:c;o=d;d=c;c=o+c|0;o=e+p|0;if(o>>>0<e>>>0){c=c+1|0}r=o;j=jb(o,c,36);o=B;p=j;j=c;p=p^jb(r,c,30);w=B^o;o=jb(r,c,25)^p;p=q&(r|x)|r&x;o=o+p|0;c=(t&(c|i)|c&i)+(B^w)|0;w=o;o=o>>>0<p>>>0?c+1|0:c;c=D|56;D=c+C|0;c=c+29744|0;u=f[c>>2];p=f[D>>2]+u|0;c=f[D+4>>2]+f[c+4>>2]|0;c=p>>>0<u>>>0?c+1|0:c;p=n+p|0;c=c+l|0;c=p>>>0<n>>>0?c+1|0:c;n=c;c=d+k|0;d=e+y|0;if(d>>>0<e>>>0){c=c+1|0}l=d;d=c;c=(h^c&(g^h))+n|0;e=v^(m^v)&l;n=e+p|0;if(n>>>0<e>>>0){c=c+1|0}e=n;n=jb(l,d,50);k=B;n=jb(l,d,46)^n;k=B^k;n=jb(l,d,23)^n;e=n+e|0;c=(B^k)+c|0;c=e>>>0<n>>>0?c+1|0:c;k=o;o=c;c=k+c|0;k=e+w|0;if(k>>>0<e>>>0){c=c+1|0}p=k;n=c;c=o+s|0;s=e+z|0;if(s>>>0<e>>>0){c=c+1|0}e=c;c=b>>>0<72;b=b+8|0;if(c){continue}break}c=h+P|0;h=v+H|0;if(h>>>0<H>>>0){c=c+1|0}b=a;f[b+200>>2]=h;f[b+204>>2]=c;c=g+Q|0;h=m+I|0;if(h>>>0<I>>>0){c=c+1|0}f[a+192>>2]=h;f[b+196>>2]=c;c=d+R|0;h=l+J|0;if(h>>>0<J>>>0){c=c+1|0}f[a+184>>2]=h;f[b+188>>2]=c;c=e+S|0;e=s+K|0;if(e>>>0<K>>>0){c=c+1|0}f[a+176>>2]=e;f[b+180>>2]=c;c=t+T|0;e=q+L|0;if(e>>>0<L>>>0){c=c+1|0}f[a+168>>2]=e;f[b+172>>2]=c;c=i+U|0;e=x+M|0;if(e>>>0<M>>>0){c=c+1|0}f[a+160>>2]=e;f[b+164>>2]=c;c=j+O|0;e=r+G|0;if(e>>>0<G>>>0){c=c+1|0}f[a+152>>2]=e;f[b+156>>2]=c;c=n+N|0;b=p+F|0;if(b>>>0<F>>>0){c=c+1|0}f[a+144>>2]=b;f[a+148>>2]=c;A=C+640|0}function Pa(a,b,c,i,j,k){var l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0;l=A-1088|0;A=l;a:{b:{switch(k+ -1|0){case 0:m=i<<8&16711680|i<<24|(i>>>8&65280|i>>>24);d[l+76|0]=m;d[l+77|0]=m>>>8;d[l+78|0]=m>>>16;d[l+79|0]=m>>>24;break a;case 1:break b;default:break a}}d[l+76|0]=i;d[l+77|0]=i>>>8;d[l+78|0]=i>>>16;d[l+79|0]=i>>>24}c:{if(c){O(l+656|0,0,131);n=h[529]|h[530]<<16;m=h[527]|h[528]<<16;e[l+814>>1]=m;e[l+816>>1]=m>>>16;e[l+818>>1]=n;e[l+820>>1]=n>>>16;m=f[263];f[l+808>>2]=f[262];f[l+812>>2]=m;m=f[261];f[l+800>>2]=f[260];f[l+804>>2]=m;X(b,c,l+800|0,l+832|0);Z(l+656|0,l+832|0,l+864|0);f[l+856>>2]=0;f[l+860>>2]=0;f[l+848>>2]=0;f[l+852>>2]=0;f[l+864>>2]=0;f[l+868>>2]=0;f[l+840>>2]=0;f[l+844>>2]=0;f[l+832>>2]=0;f[l+836>>2]=0;Y(l+560|0,l+656|0,a,64);O(l+656|0,0,131);break c}m=a;n=g[m+60|0]|g[m+61|0]<<8|(g[m+62|0]<<16|g[m+63|0]<<24);f[l+616>>2]=g[m+56|0]|g[m+57|0]<<8|(g[m+58|0]<<16|g[m+59|0]<<24);f[l+620>>2]=n;n=g[m+52|0]|g[m+53|0]<<8|(g[m+54|0]<<16|g[m+55|0]<<24);f[l+608>>2]=g[m+48|0]|g[m+49|0]<<8|(g[m+50|0]<<16|g[m+51|0]<<24);f[l+612>>2]=n;n=g[m+44|0]|g[m+45|0]<<8|(g[m+46|0]<<16|g[m+47|0]<<24);f[l+600>>2]=g[m+40|0]|g[m+41|0]<<8|(g[m+42|0]<<16|g[m+43|0]<<24);f[l+604>>2]=n;n=g[m+36|0]|g[m+37|0]<<8|(g[m+38|0]<<16|g[m+39|0]<<24);f[l+592>>2]=g[m+32|0]|g[m+33|0]<<8|(g[m+34|0]<<16|g[m+35|0]<<24);f[l+596>>2]=n;n=g[m+28|0]|g[m+29|0]<<8|(g[m+30|0]<<16|g[m+31|0]<<24);f[l+584>>2]=g[m+24|0]|g[m+25|0]<<8|(g[m+26|0]<<16|g[m+27|0]<<24);f[l+588>>2]=n;n=g[m+20|0]|g[m+21|0]<<8|(g[m+22|0]<<16|g[m+23|0]<<24);f[l+576>>2]=g[m+16|0]|g[m+17|0]<<8|(g[m+18|0]<<16|g[m+19|0]<<24);f[l+580>>2]=n;n=g[m+4|0]|g[m+5|0]<<8|(g[m+6|0]<<16|g[m+7|0]<<24);f[l+560>>2]=g[m|0]|g[m+1|0]<<8|(g[m+2|0]<<16|g[m+3|0]<<24);f[l+564>>2]=n;n=g[m+12|0]|g[m+13|0]<<8|(g[m+14|0]<<16|g[m+15|0]<<24);f[l+568>>2]=g[m+8|0]|g[m+9|0]<<8|(g[m+10|0]<<16|g[m+11|0]<<24);f[l+572>>2]=n}m=a;n=g[m+116|0]|g[m+117|0]<<8|(g[m+118|0]<<16|g[m+119|0]<<24);f[l+672>>2]=g[m+112|0]|g[m+113|0]<<8|(g[m+114|0]<<16|g[m+115|0]<<24);f[l+676>>2]=n;n=g[m+124|0]|g[m+125|0]<<8|(g[m+126|0]<<16|g[m+127|0]<<24);f[l+680>>2]=g[m+120|0]|g[m+121|0]<<8|(g[m+122|0]<<16|g[m+123|0]<<24);f[l+684>>2]=n;n=g[m+100|0]|g[m+101|0]<<8|(g[m+102|0]<<16|g[m+103|0]<<24);f[l+656>>2]=g[m+96|0]|g[m+97|0]<<8|(g[m+98|0]<<16|g[m+99|0]<<24);f[l+660>>2]=n;n=g[m+108|0]|g[m+109|0]<<8|(g[m+110|0]<<16|g[m+111|0]<<24);f[l+664>>2]=g[m+104|0]|g[m+105|0]<<8|(g[m+106|0]<<16|g[m+107|0]<<24);f[l+668>>2]=n;q=O(l+688|0,0,96);m=m+96|0;while(1){n=g[(l+656|0)+o|0];d[(l+960|0)+o|0]=n^54;d[(l+832|0)+o|0]=n^92;o=o+1|0;if((o|0)!=128){continue}break}R(l+80|0);N(l+80|0,l+960|0,128);p=l+288|0;R(p);N(p,l+832|0,128);d:{if((i|0)<=-1){N(l+80|0,1024,1);N(l+80|0,l+560|0,64);break d}N(l+80|0,1026,1);N(l+80|0,a- -64|0,32)}N(l+80|0,l+76|0,4);Q(l+80|0,l);N(p,l,64);Q(p,l);f[l+648>>2]=0;f[l+652>>2]=0;f[l+640>>2]=0;f[l+644>>2]=0;f[l+632>>2]=0;f[l+636>>2]=0;f[l+624>>2]=0;f[l+628>>2]=0;e:{f:{g:{h:{i:{j:{n=k+ -1|0;switch(n|0){case 0:break h;case 1:break j;default:break i}}ma(l+624|0,l);o=0;k=0;while(1){k=g[(l+560|0)+o|0]+(g[(l+624|0)+o|0]+k|0)|0;d[(l+496|0)+o|0]=k;k=k>>>8|0;o=o+1|0;if((o|0)!=32){continue}break}}switch(n|0){case 1:break f;case 0:break g;default:break e}}d[l+624|0]=g[l|0]<<3;d[l+625|0]=g[l+1|0]<<3;d[l+626|0]=g[l+2|0]<<3;d[l+627|0]=g[l+3|0]<<3;d[l+628|0]=g[l+4|0]<<3;d[l+629|0]=g[l+5|0]<<3;d[l+630|0]=g[l+6|0]<<3;d[l+631|0]=g[l+7|0]<<3;d[l+632|0]=g[l+8|0]<<3;d[l+633|0]=g[l+9|0]<<3;d[l+634|0]=g[l+10|0]<<3;d[l+635|0]=g[l+11|0]<<3;d[l+636|0]=g[l+12|0]<<3;d[l+637|0]=g[l+13|0]<<3;d[l+638|0]=g[l+14|0]<<3;d[l+639|0]=g[l+15|0]<<3;d[l+640|0]=g[l+16|0]<<3;d[l+641|0]=g[l+17|0]<<3;d[l+642|0]=g[l+18|0]<<3;d[l+643|0]=g[l+19|0]<<3;d[l+644|0]=g[l+20|0]<<3;d[l+645|0]=g[l+21|0]<<3;d[l+646|0]=g[l+22|0]<<3;d[l+647|0]=g[l+23|0]<<3;d[l+648|0]=g[l+24|0]<<3;d[l+649|0]=g[l+25|0]<<3;d[l+650|0]=g[l+26|0]<<3;d[l+651|0]=g[l+27|0]<<3;d[l+652|0]=g[l+28|0]<<3;d[l+653|0]=g[l+29|0]<<3;d[l+654|0]=g[l+30|0]<<3;d[l+655|0]=g[l+31|0]<<3;Oa(l+624|0,l+560|0,l+496|0)}d[l+528|0]=g[l+592|0]+g[l+32|0];d[l+529|0]=g[l+593|0]+g[l+33|0];d[l+530|0]=g[l+594|0]+g[l+34|0];d[l+531|0]=g[l+595|0]+g[l+35|0];d[l+532|0]=g[l+596|0]+g[l+36|0];d[l+533|0]=g[l+597|0]+g[l+37|0];d[l+534|0]=g[l+598|0]+g[l+38|0];d[l+535|0]=g[l+599|0]+g[l+39|0];d[l+536|0]=g[l+600|0]+g[l+40|0];d[l+537|0]=g[l+601|0]+g[l+41|0];d[l+538|0]=g[l+602|0]+g[l+42|0];d[l+539|0]=g[l+603|0]+g[l+43|0];d[l+540|0]=g[l+604|0]+g[l+44|0];d[l+541|0]=g[l+605|0]+g[l+45|0];d[l+542|0]=g[l+606|0]+g[l+46|0];d[l+543|0]=g[l+607|0]+g[l+47|0];d[l+544|0]=g[l+608|0]+g[l+48|0];d[l+545|0]=g[l+609|0]+g[l+49|0];d[l+546|0]=g[l+610|0]+g[l+50|0];d[l+547|0]=g[l+611|0]+g[l+51|0];d[l+548|0]=g[l+612|0]+g[l+52|0];d[l+549|0]=g[l+613|0]+g[l+53|0];d[l+550|0]=g[l+614|0]+g[l+54|0];d[l+551|0]=g[l+615|0]+g[l+55|0];d[l+552|0]=g[l+616|0]+g[l+56|0];d[l+553|0]=g[l+617|0]+g[l+57|0];d[l+554|0]=g[l+618|0]+g[l+58|0];d[l+555|0]=g[l+619|0]+g[l+59|0];d[l+556|0]=g[l+620|0]+g[l+60|0];d[l+557|0]=g[l+621|0]+g[l+61|0];d[l+558|0]=g[l+622|0]+g[l+62|0];d[l+559|0]=g[l+623|0]+g[l+63|0];break e}r=l+592|0;s=l+32|0;n=l+528|0;o=0;k=0;while(1){k=g[o+r|0]+(g[o+s|0]+k|0)|0;d[n+o|0]=k;k=k>>>0>255;o=o+1|0;if((o|0)!=32){continue}break}}k=g[m+20|0]|g[m+21|0]<<8|(g[m+22|0]<<16|g[m+23|0]<<24);f[l+672>>2]=g[m+16|0]|g[m+17|0]<<8|(g[m+18|0]<<16|g[m+19|0]<<24);f[l+676>>2]=k;k=g[m+28|0]|g[m+29|0]<<8|(g[m+30|0]<<16|g[m+31|0]<<24);f[l+680>>2]=g[m+24|0]|g[m+25|0]<<8|(g[m+26|0]<<16|g[m+27|0]<<24);f[l+684>>2]=k;k=g[m+4|0]|g[m+5|0]<<8|(g[m+6|0]<<16|g[m+7|0]<<24);f[l+656>>2]=g[m|0]|g[m+1|0]<<8|(g[m+2|0]<<16|g[m+3|0]<<24);f[l+660>>2]=k;k=g[m+12|0]|g[m+13|0]<<8|(g[m+14|0]<<16|g[m+15|0]<<24);f[l+664>>2]=g[m+8|0]|g[m+9|0]<<8|(g[m+10|0]<<16|g[m+11|0]<<24);f[l+668>>2]=k;o=0;O(q,0,96);while(1){k=g[(l+656|0)+o|0];d[(l+960|0)+o|0]=k^54;d[(l+832|0)+o|0]=k^92;o=o+1|0;if((o|0)!=128){continue}break}R(l+80|0);N(l+80|0,l+960|0,128);R(p);N(p,l+832|0,128);k:{if((i|0)<=-1){N(l+80|0,1028,1);N(l+80|0,l+560|0,64);break k}N(l+80|0,1030,1);N(l+80|0,a- -64|0,32)}N(l+80|0,l+76|0,4);Q(l+80|0,l+960|0);N(p,l+960|0,64);Q(p,l+960|0);f[l+616>>2]=0;f[l+620>>2]=0;f[l+608>>2]=0;f[l+612>>2]=0;f[l+600>>2]=0;f[l+604>>2]=0;f[l+592>>2]=0;f[l+596>>2]=0;f[l+584>>2]=0;f[l+588>>2]=0;f[l+576>>2]=0;f[l+580>>2]=0;f[l+568>>2]=0;f[l+572>>2]=0;f[l+560>>2]=0;f[l+564>>2]=0;ba(l+496|0,l+624|0);l:{if(c){O(l+656|0,0,131);i=h[529]|h[530]<<16;a=h[527]|h[528]<<16;e[l+814>>1]=a;e[l+816>>1]=a>>>16;e[l+818>>1]=i;e[l+820>>1]=i>>>16;a=f[263];f[l+808>>2]=f[262];f[l+812>>2]=a;a=f[261];f[l+800>>2]=f[260];f[l+804>>2]=a;X(b,c,l+800|0,l+832|0);Z(l+656|0,l+832|0,l+864|0);f[l+856>>2]=0;f[l+860>>2]=0;f[l+848>>2]=0;f[l+852>>2]=0;f[l+864>>2]=0;f[l+868>>2]=0;f[l+840>>2]=0;f[l+844>>2]=0;f[l+832>>2]=0;f[l+836>>2]=0;Y(j,l+656|0,l+496|0,64);O(l+656|0,0,131);break l}b=f[l+500>>2];a=f[l+496>>2];d[j|0]=a;d[j+1|0]=a>>>8;d[j+2|0]=a>>>16;d[j+3|0]=a>>>24;d[j+4|0]=b;d[j+5|0]=b>>>8;d[j+6|0]=b>>>16;d[j+7|0]=b>>>24;b=f[l+508>>2];a=f[l+504>>2];d[j+8|0]=a;d[j+9|0]=a>>>8;d[j+10|0]=a>>>16;d[j+11|0]=a>>>24;d[j+12|0]=b;d[j+13|0]=b>>>8;d[j+14|0]=b>>>16;d[j+15|0]=b>>>24;b=f[l+556>>2];a=f[l+552>>2];d[j+56|0]=a;d[j+57|0]=a>>>8;d[j+58|0]=a>>>16;d[j+59|0]=a>>>24;d[j+60|0]=b;d[j+61|0]=b>>>8;d[j+62|0]=b>>>16;d[j+63|0]=b>>>24;b=f[l+548>>2];a=f[l+544>>2];d[j+48|0]=a;d[j+49|0]=a>>>8;d[j+50|0]=a>>>16;d[j+51|0]=a>>>24;d[j+52|0]=b;d[j+53|0]=b>>>8;d[j+54|0]=b>>>16;d[j+55|0]=b>>>24;b=f[l+540>>2];a=f[l+536>>2];d[j+40|0]=a;d[j+41|0]=a>>>8;d[j+42|0]=a>>>16;d[j+43|0]=a>>>24;d[j+44|0]=b;d[j+45|0]=b>>>8;d[j+46|0]=b>>>16;d[j+47|0]=b>>>24;b=f[l+532>>2];a=f[l+528>>2];d[j+32|0]=a;d[j+33|0]=a>>>8;d[j+34|0]=a>>>16;d[j+35|0]=a>>>24;d[j+36|0]=b;d[j+37|0]=b>>>8;d[j+38|0]=b>>>16;d[j+39|0]=b>>>24;b=f[l+524>>2];a=f[l+520>>2];d[j+24|0]=a;d[j+25|0]=a>>>8;d[j+26|0]=a>>>16;d[j+27|0]=a>>>24;d[j+28|0]=b;d[j+29|0]=b>>>8;d[j+30|0]=b>>>16;d[j+31|0]=b>>>24;b=f[l+516>>2];a=f[l+512>>2];d[j+16|0]=a;d[j+17|0]=a>>>8;d[j+18|0]=a>>>16;d[j+19|0]=a>>>24;d[j+20|0]=b;d[j+21|0]=b>>>8;d[j+22|0]=b>>>16;d[j+23|0]=b>>>24}b=f[l+636>>2];a=f[l+632>>2];d[j+72|0]=a;d[j+73|0]=a>>>8;d[j+74|0]=a>>>16;d[j+75|0]=a>>>24;d[j+76|0]=b;d[j+77|0]=b>>>8;d[j+78|0]=b>>>16;d[j+79|0]=b>>>24;b=f[l+628>>2];a=f[l+624>>2];d[j+64|0]=a;d[j+65|0]=a>>>8;d[j+66|0]=a>>>16;d[j+67|0]=a>>>24;d[j+68|0]=b;d[j+69|0]=b>>>8;d[j+70|0]=b>>>16;d[j+71|0]=b>>>24;b=f[l+652>>2];a=f[l+648>>2];d[j+88|0]=a;d[j+89|0]=a>>>8;d[j+90|0]=a>>>16;d[j+91|0]=a>>>24;d[j+92|0]=b;d[j+93|0]=b>>>8;d[j+94|0]=b>>>16;d[j+95|0]=b>>>24;b=f[l+644>>2];a=f[l+640>>2];d[j+80|0]=a;d[j+81|0]=a>>>8;d[j+82|0]=a>>>16;d[j+83|0]=a>>>24;d[j+84|0]=b;d[j+85|0]=b>>>8;d[j+86|0]=b>>>16;d[j+87|0]=b>>>24;b=f[l+996>>2];a=f[l+992>>2];d[j+96|0]=a;d[j+97|0]=a>>>8;d[j+98|0]=a>>>16;d[j+99|0]=a>>>24;d[j+100|0]=b;d[j+101|0]=b>>>8;d[j+102|0]=b>>>16;d[j+103|0]=b>>>24;b=f[l+1004>>2];a=f[l+1e3>>2];d[j+104|0]=a;d[j+105|0]=a>>>8;d[j+106|0]=a>>>16;d[j+107|0]=a>>>24;d[j+108|0]=b;d[j+109|0]=b>>>8;d[j+110|0]=b>>>16;d[j+111|0]=b>>>24;b=f[l+1012>>2];a=f[l+1008>>2];d[j+112|0]=a;d[j+113|0]=a>>>8;d[j+114|0]=a>>>16;d[j+115|0]=a>>>24;d[j+116|0]=b;d[j+117|0]=b>>>8;d[j+118|0]=b>>>16;d[j+119|0]=b>>>24;b=f[l+1020>>2];a=f[l+1016>>2];d[j+120|0]=a;d[j+121|0]=a>>>8;d[j+122|0]=a>>>16;d[j+123|0]=a>>>24;d[j+124|0]=b;d[j+125|0]=b>>>8;d[j+126|0]=b>>>16;d[j+127|0]=b>>>24;A=l+1088|0}



function Ra(a,b,c,e,h){var i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0;k=A-960|0;A=k;l=g[c+20|0]|g[c+21|0]<<8|(g[c+22|0]<<16|g[c+23|0]<<24);f[k+144>>2]=g[c+16|0]|g[c+17|0]<<8|(g[c+18|0]<<16|g[c+19|0]<<24);f[k+148>>2]=l;l=g[c+28|0]|g[c+29|0]<<8|(g[c+30|0]<<16|g[c+31|0]<<24);f[k+152>>2]=g[c+24|0]|g[c+25|0]<<8|(g[c+26|0]<<16|g[c+27|0]<<24);f[k+156>>2]=l;j=g[c+36|0]|g[c+37|0]<<8|(g[c+38|0]<<16|g[c+39|0]<<24);l=k+160|0;i=l;f[i>>2]=g[c+32|0]|g[c+33|0]<<8|(g[c+34|0]<<16|g[c+35|0]<<24);f[i+4>>2]=j;i=g[c+44|0]|g[c+45|0]<<8|(g[c+46|0]<<16|g[c+47|0]<<24);f[k+168>>2]=g[c+40|0]|g[c+41|0]<<8|(g[c+42|0]<<16|g[c+43|0]<<24);f[k+172>>2]=i;i=g[c+52|0]|g[c+53|0]<<8|(g[c+54|0]<<16|g[c+55|0]<<24);f[k+176>>2]=g[c+48|0]|g[c+49|0]<<8|(g[c+50|0]<<16|g[c+51|0]<<24);f[k+180>>2]=i;i=g[c+60|0]|g[c+61|0]<<8|(g[c+62|0]<<16|g[c+63|0]<<24);f[k+184>>2]=g[c+56|0]|g[c+57|0]<<8|(g[c+58|0]<<16|g[c+59|0]<<24);f[k+188>>2]=i;i=g[c+4|0]|g[c+5|0]<<8|(g[c+6|0]<<16|g[c+7|0]<<24);f[k+128>>2]=g[c|0]|g[c+1|0]<<8|(g[c+2|0]<<16|g[c+3|0]<<24);f[k+132>>2]=i;i=g[c+12|0]|g[c+13|0]<<8|(g[c+14|0]<<16|g[c+15|0]<<24);f[k+136>>2]=g[c+8|0]|g[c+9|0]<<8|(g[c+10|0]<<16|g[c+11|0]<<24);f[k+140>>2]=i;R(k+496|0);N(k+496|0,l,32);N(k+496|0,a,b);Q(k+496|0,k- -64|0);V(k+448|0,k- -64|0,64);sa(k+192|0,k+448|0);da(h,k+192|0);R(k+704|0);N(k+704|0,h,32);N(k+704|0,e,32);N(k+704|0,a,b);Q(k+704|0,k);V(k+400|0,k,64);V(k+352|0,k+128|0,32);l=f[k+352>>2];i=f[k+400>>2];a=hb(l,0,i);b=B;f[k+704>>2]=a&1073741823;e=a;a=b>>>30|0;c=k;j=f[c+356>>2];e=(b&1073741823)<<2|e>>>30;b=hb(i,0,j)+e|0;a=a+B|0;a=b>>>0<e>>>0?a+1|0:a;m=f[c+404>>2];e=hb(l,0,m);b=e+b|0;a=B+a|0;y=b;b=b>>>0<e>>>0?a+1|0:a;f[c+708>>2]=y&1073741823;z=f[c+360>>2];a=hb(i,0,z);v=B;u=hb(m,0,j);e=u+a|0;a=B+v|0;v=e;e=e>>>0<u>>>0?a+1|0:a;a=b>>>30|0;y=(b&1073741823)<<2|y>>>30;b=y+v|0;a=a+e|0;a=b>>>0<y>>>0?a+1|0:a;y=f[c+408>>2];e=hb(l,0,y);b=e+b|0;a=B+a|0;r=b;b=b>>>0<e>>>0?a+1|0:a;f[c+712>>2]=r&1073741823;a=hb(y,0,j);v=B;u=hb(m,0,z);e=u+a|0;a=B+v|0;a=e>>>0<u>>>0?a+1|0:a;v=f[c+364>>2];u=hb(i,0,v);e=u+e|0;a=B+a|0;a=e>>>0<u>>>0?a+1|0:a;u=f[c+412>>2];s=hb(l,0,u);e=s+e|0;a=B+a|0;a=e>>>0<s>>>0?a+1|0:a;s=e;e=a;a=b>>>30|0;r=(b&1073741823)<<2|r>>>30;b=r+s|0;a=a+e|0;p=b;b=b>>>0<r>>>0?a+1|0:a;f[c+716>>2]=p&1073741823;a=hb(m,0,v);r=B;s=hb(y,0,z);e=s+a|0;a=B+r|0;a=e>>>0<s>>>0?a+1|0:a;r=hb(j,0,u);e=r+e|0;a=B+a|0;a=e>>>0<r>>>0?a+1|0:a;r=f[c+368>>2];s=hb(i,0,r);e=s+e|0;a=B+a|0;a=e>>>0<s>>>0?a+1|0:a;s=f[c+416>>2];t=hb(l,0,s);e=t+e|0;a=B+a|0;a=e>>>0<t>>>0?a+1|0:a;t=e;e=a;a=b>>>30|0;p=(b&1073741823)<<2|p>>>30;b=p+t|0;a=a+e|0;q=b;b=b>>>0<p>>>0?a+1|0:a;f[c+720>>2]=q&1073741823;a=hb(z,0,u);p=B;t=hb(y,0,v);e=t+a|0;a=B+p|0;a=e>>>0<t>>>0?a+1|0:a;p=hb(m,0,r);e=p+e|0;a=B+a|0;a=e>>>0<p>>>0?a+1|0:a;p=hb(j,0,s);e=p+e|0;a=B+a|0;a=e>>>0<p>>>0?a+1|0:a;p=f[c+372>>2];t=hb(i,0,p);e=t+e|0;a=B+a|0;a=e>>>0<t>>>0?a+1|0:a;t=f[c+420>>2];w=hb(l,0,t);e=w+e|0;a=B+a|0;a=e>>>0<w>>>0?a+1|0:a;w=e;e=a;a=b>>>30|0;q=(b&1073741823)<<2|q>>>30;b=q+w|0;a=a+e|0;o=b;b=b>>>0<q>>>0?a+1|0:a;f[c+724>>2]=o&1073741823;a=hb(y,0,r);q=B;w=hb(v,0,u);e=w+a|0;a=B+q|0;a=e>>>0<w>>>0?a+1|0:a;q=hb(z,0,s);e=q+e|0;a=B+a|0;a=e>>>0<q>>>0?a+1|0:a;q=hb(m,0,p);e=q+e|0;a=B+a|0;a=e>>>0<q>>>0?a+1|0:a;q=hb(j,0,t);e=q+e|0;a=B+a|0;a=e>>>0<q>>>0?a+1|0:a;q=f[c+376>>2];w=hb(i,0,q);e=w+e|0;a=B+a|0;a=e>>>0<w>>>0?a+1|0:a;w=f[c+424>>2];x=hb(l,0,w);e=x+e|0;a=B+a|0;a=e>>>0<x>>>0?a+1|0:a;x=e;e=a;a=b>>>30|0;o=(b&1073741823)<<2|o>>>30;b=o+x|0;a=a+e|0;n=b;b=b>>>0<o>>>0?a+1|0:a;f[c+728>>2]=n&1073741823;a=hb(v,0,s);o=B;x=hb(r,0,u);e=x+a|0;a=B+o|0;a=e>>>0<x>>>0?a+1|0:a;o=hb(y,0,p);e=o+e|0;a=B+a|0;a=e>>>0<o>>>0?a+1|0:a;o=hb(z,0,t);e=o+e|0;a=B+a|0;a=e>>>0<o>>>0?a+1|0:a;o=hb(m,0,q);e=o+e|0;a=B+a|0;a=e>>>0<o>>>0?a+1|0:a;o=hb(j,0,w);e=o+e|0;a=B+a|0;a=e>>>0<o>>>0?a+1|0:a;o=f[c+380>>2];x=hb(i,0,o);e=x+e|0;a=B+a|0;a=e>>>0<x>>>0?a+1|0:a;x=f[c+428>>2];C=hb(l,0,x);e=C+e|0;a=B+a|0;a=e>>>0<C>>>0?a+1|0:a;C=e;e=a;a=b>>>30|0;n=(b&1073741823)<<2|n>>>30;b=n+C|0;a=a+e|0;D=b;b=b>>>0<n>>>0?a+1|0:a;f[c+732>>2]=D&1073741823;e=c;a=hb(p,0,u);n=B;C=hb(r,0,s);c=C+a|0;a=B+n|0;a=c>>>0<C>>>0?a+1|0:a;n=hb(v,0,t);c=n+c|0;a=B+a|0;a=c>>>0<n>>>0?a+1|0:a;n=hb(y,0,q);c=n+c|0;a=B+a|0;a=c>>>0<n>>>0?a+1|0:a;n=hb(z,0,w);c=n+c|0;a=B+a|0;a=c>>>0<n>>>0?a+1|0:a;n=hb(m,0,o);c=n+c|0;a=B+a|0;a=c>>>0<n>>>0?a+1|0:a;n=hb(j,0,x);c=n+c|0;a=B+a|0;a=c>>>0<n>>>0?a+1|0:a;n=f[e+384>>2];i=hb(i,0,n);c=i+c|0;a=B+a|0;a=c>>>0<i>>>0?a+1|0:a;C=f[e+432>>2];l=hb(l,0,C);c=l+c|0;a=B+a|0;a=c>>>0<l>>>0?a+1|0:a;l=c;c=a;e=l;a=b>>>30|0;l=(b&1073741823)<<2|D>>>30;b=e+l|0;a=a+c|0;c=b;b=c>>>0<l>>>0?a+1|0:a;e=c;f[k+736>>2]=c&16777215;a=hb(r,0,t);D=B;E=hb(p,0,s);i=E+a|0;a=B+D|0;a=i>>>0<E>>>0?a+1|0:a;D=hb(q,0,u);i=D+i|0;a=B+a|0;a=i>>>0<D>>>0?a+1|0:a;D=hb(v,0,w);i=D+i|0;a=B+a|0;a=i>>>0<D>>>0?a+1|0:a;D=hb(y,0,o);i=D+i|0;a=B+a|0;a=i>>>0<D>>>0?a+1|0:a;D=hb(z,0,x);i=D+i|0;a=B+a|0;a=i>>>0<D>>>0?a+1|0:a;m=hb(m,0,n);i=m+i|0;a=B+a|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(C,0,j);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;l=c;f[k+912>>2]=c<<22&1069547520|e>>>8&4194303;a=hb(q,0,s);j=B;m=hb(p,0,t);i=m+a|0;a=B+j|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(r,0,w);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(o,0,u);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(v,0,x);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(y,0,n);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(C,0,z);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;e=c;f[k+916>>2]=c<<22&1069547520|l>>>8&4194303;a=hb(p,0,w);j=B;m=hb(q,0,t);i=m+a|0;a=B+j|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(o,0,s);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(r,0,x);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(n,0,u);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(C,0,v);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;l=c;f[k+920>>2]=c<<22&1069547520|e>>>8&4194303;a=hb(o,0,t);j=B;m=hb(q,0,w);i=m+a|0;a=B+j|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(p,0,x);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(n,0,s);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(C,0,r);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;e=c;f[k+924>>2]=c<<22&1069547520|l>>>8&4194303;a=hb(q,0,x);j=B;m=hb(o,0,w);i=m+a|0;a=B+j|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(n,0,t);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=hb(C,0,p);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;l=c;f[k+928>>2]=c<<22&1069547520|e>>>8&4194303;a=hb(n,0,w);j=B;m=hb(o,0,x);i=m+a|0;a=B+j|0;a=i>>>0<m>>>0?a+1|0:a;j=hb(C,0,q);i=j+i|0;a=B+a|0;a=i>>>0<j>>>0?a+1|0:a;j=i;i=a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;b=a;e=c;f[k+932>>2]=c<<22&1069547520|l>>>8&4194303;a=hb(C,0,o);j=B;m=hb(n,0,x);i=m+a|0;a=B+j|0;j=i;i=i>>>0<m>>>0?a+1|0:a;a=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+j|0;a=a+i|0;a=b>>>0<c>>>0?a+1|0:a;c=b;f[k+936>>2]=c<<22&1069547520|e>>>8&4194303;b=hb(C,0,n)+((a&1073741823)<<2|c>>>30)|0;a=b;f[k+944>>2]=a>>>8&4194303;f[k+940>>2]=a<<22&1069547520|c>>>8&4194303;ra(k+400|0,k+912|0,k+704|0);a=f[k+412>>2];c=f[k+460>>2];e=f[k+404>>2];l=f[k+452>>2];i=f[k+408>>2];j=f[k+456>>2];m=f[k+448>>2]+f[k+400>>2]|0;b=m&1073741823;f[k+400>>2]=b;e=l+(e+(m>>>30|0)|0)|0;m=e&1073741823;f[k+404>>2]=m;e=j+(i+(e>>>30|0)|0)|0;j=e&1073741823;f[k+408>>2]=j;a=a+c+(e>>>30)|0;z=f[k+464>>2]+f[k+416>>2]+(a>>>30)|0;i=f[k+468>>2]+f[k+420>>2]+(z>>>30)|0;l=f[k+472>>2]+f[k+424>>2]+(i>>>30)|0;e=f[k+476>>2]+f[k+428>>2]+(l>>>30)|0;c=f[k+480>>2]+f[k+432>>2]+(e>>>30)|0;e=e&1073741823;l=l&1073741823;i=i&1073741823;z=z&1073741823;y=a&1073741823;v=b+ -485872621|0;u=(m+(v>>31)|0)+ -541690985|0;r=(j+(u>>31)|0)+ -796511589|0;s=r>>>31|0;p=y-(s|935229352)|0;t=p>>>31|0;q=z-(t|20)|0;w=q>>>31|0;o=i-w|0;x=o>>>31|0;n=l-x|0;C=n>>>31|0;D=e-C|0;E=D>>>31|0;F=c-(E|4096)|0;G=F>>>31|0;a=G+ -1|0;b=b^a&(b^v+(v>>>1&1073741824));f[k+400>>2]=b;v=c^a&(c^(G<<16)+F);f[k+432>>2]=v;c=e^a&(e^(E<<30)+D);f[k+428>>2]=c;e=l^a&(l^(C<<30)+n);f[k+424>>2]=e;l=i^a&(i^(x<<30)+o);f[k+420>>2]=l;z=z^a&(z^(w<<30)+q);f[k+416>>2]=z;i=y^a&(y^p+(t<<30));f[k+412>>2]=i;j=j^a&(j^r+(s<<30));f[k+408>>2]=j;a=m^a&(m^u+(u>>>1&1073741824));f[k+404>>2]=a;d[h+33|0]=b>>>8;d[h+34|0]=b>>>16;m=a>>>2|0;d[h+36|0]=m;d[h+37|0]=a>>>10;d[h+38|0]=a>>>18;y=j>>>4|0;d[h+40|0]=y;d[h+41|0]=j>>>12;d[h+42|0]=j>>>20;u=i>>>6|0;d[h+44|0]=u;d[h+45|0]=i>>>14;d[h+46|0]=i>>>22;r=z>>>8|0;d[h+48|0]=r;d[h+49|0]=z>>>16;d[h+51|0]=l>>>2;s=l>>>10|0;d[h+52|0]=s;d[h+53|0]=l>>>18;d[h+55|0]=e>>>4;p=e>>>12|0;d[h+56|0]=p;d[h+57|0]=e>>>20;d[h+59|0]=c>>>6;t=c>>>14|0;d[h+60|0]=t;d[h+61|0]=c>>>22;d[h+32|0]=b;d[h+35|0]=(b|a<<30)>>>24;d[h+39|0]=(m|j<<28)>>>24;d[h+43|0]=(y|i<<26)>>>24;d[h+47|0]=(u|z<<24)>>>24;d[h+50|0]=(r|l<<22)>>>16;d[h+54|0]=(s|e<<20)>>>16;d[h+58|0]=(p|c<<18)>>>16;d[h+63|0]=v>>>8;d[h+62|0]=(t|v<<16)>>>16;A=k+960|0}function X(a,b,c,e){var h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,v=0,w=0,x=0,y=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0;h=A-1104|0;A=h;if(e){a:{b:{if(b>>>0>=129){R(h);N(h,a,b);Q(h,h+544|0);b=64;break b}if((h+544|0)!=(a|0)){P(h+544|0,a,b)}if(b>>>0>127){break a}}O((h+544|0)+b|0,0,128-b|0)}b=0;while(1){a=g[(h+544|0)+b|0];d[(h+672|0)+b|0]=a^54;d[(h+416|0)+b|0]=a^92;b=b+1|0;if((b|0)!=128){continue}break}R(h);N(h,h+672|0,128);a=h+208|0;R(a);N(a,h+416|0,128);M=h+880|0;a=1;while(1){b=a<<8&16711680|a<<24|(a>>>8&65280|a>>>24);d[h+1100|0]=b;d[h+1101|0]=b>>>8;d[h+1102|0]=b>>>16;d[h+1103|0]=b>>>24;f[h+664>>2]=0;f[h+656>>2]=0;f[h+660>>2]=0;f[h+648>>2]=0;f[h+652>>2]=0;f[h+640>>2]=0;f[h+644>>2]=0;f[h+632>>2]=0;f[h+636>>2]=0;f[h+624>>2]=0;f[h+628>>2]=0;f[h+616>>2]=0;f[h+620>>2]=0;f[h+608>>2]=0;f[h+612>>2]=0;d[h+608|0]=128;f[h+668>>2]=393216;P(h+672|0,h,416);N(h+672|0,c,22);N(h+672|0,h+1100|0,4);Q(h+672|0,h+544|0);N(M,h+544|0,64);Q(M,h+544|0);E=f[h+1080>>2];S=f[h+1084>>2];F=f[h+1072>>2];T=f[h+1076>>2];G=f[h+1064>>2];U=f[h+1068>>2];H=f[h+1056>>2];V=f[h+1060>>2];I=f[h+1048>>2];W=f[h+1052>>2];J=f[h+1040>>2];X=f[h+1044>>2];K=f[h+1032>>2];Y=f[h+1036>>2];C=f[h+1024>>2];Z=f[h+1028>>2];b=1;while(1){i=f[h+148>>2];f[h+816>>2]=f[h+144>>2];f[h+820>>2]=i;i=f[h+156>>2];f[h+824>>2]=f[h+152>>2];f[h+828>>2]=i;i=f[h+164>>2];f[h+832>>2]=f[h+160>>2];f[h+836>>2]=i;i=f[h+172>>2];f[h+840>>2]=f[h+168>>2];f[h+844>>2]=i;i=f[h+180>>2];f[h+848>>2]=f[h+176>>2];f[h+852>>2]=i;i=f[h+188>>2];f[h+856>>2]=f[h+184>>2];f[h+860>>2]=i;i=f[h+196>>2];f[h+864>>2]=f[h+192>>2];f[h+868>>2]=i;i=f[h+204>>2];f[h+872>>2]=f[h+200>>2];f[h+876>>2]=i;N(h+672|0,h+544|0,128);k=f[h+816>>2];i=k<<24;j=k<<8;n=0;l=j&16711680|i;m=f[h+820>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+544>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+548>>2]=l;k=f[h+824>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+828>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+552>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+556>>2]=l;k=f[h+832>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+836>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+560>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+564>>2]=l;k=f[h+840>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+844>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+568>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+572>>2]=l;k=f[h+848>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+852>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+576>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+580>>2]=l;k=f[h+856>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+860>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+584>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+588>>2]=l;k=f[h+864>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+868>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+592>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+596>>2]=l;k=f[h+872>>2];i=k<<24;j=k<<8;l=j&16711680|i;m=f[h+876>>2];j=m<<24|k>>>8;i=m<<8|k>>>24;l=j&65280|i&255|l;i=m;f[h+600>>2]=((i&255)<<24|k>>>8)&-16777216|((i&16777215)<<8|k>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+604>>2]=l;i=f[h+356>>2];f[h+1024>>2]=f[h+352>>2];f[h+1028>>2]=i;i=f[h+364>>2];f[h+1032>>2]=f[h+360>>2];f[h+1036>>2]=i;i=f[h+372>>2];f[h+1040>>2]=f[h+368>>2];f[h+1044>>2]=i;i=f[h+380>>2];f[h+1048>>2]=f[h+376>>2];f[h+1052>>2]=i;i=f[h+388>>2];f[h+1056>>2]=f[h+384>>2];f[h+1060>>2]=i;i=f[h+396>>2];f[h+1064>>2]=f[h+392>>2];f[h+1068>>2]=i;i=f[h+404>>2];f[h+1072>>2]=f[h+400>>2];f[h+1076>>2]=i;i=f[h+412>>2];f[h+1080>>2]=f[h+408>>2];f[h+1084>>2]=i;N(M,h+544|0,128);s=f[h+1024>>2];i=s<<24;j=s<<8;k=0;n=j&16711680|i;m=f[h+1028>>2];j=m<<24|s>>>8;i=m<<8|s>>>24;n=j&65280|i&255|n;i=m;f[h+544>>2]=((i&255)<<24|s>>>8)&-16777216|((i&16777215)<<8|s>>>24)&16711680|(i>>>8&65280|i>>>24)|k;f[h+548>>2]=n;t=f[h+1032>>2];i=t<<24;j=t<<8;n=0;l=j&16711680|i;L=f[h+1036>>2];j=L<<24|t>>>8;i=L<<8|t>>>24;l=j&65280|i&255|l;i=L;f[h+552>>2]=((i&255)<<24|t>>>8)&-16777216|((i&16777215)<<8|t>>>24)&16711680|(i>>>8&65280|i>>>24)|n;f[h+556>>2]=l;v=f[h+1040>>2];i=v<<24;j=v<<8;l=0;o=j&16711680|i;k=f[h+1044>>2];j=k<<24|v>>>8;i=k<<8|v>>>24;o=j&65280|i&255|o;i=k;f[h+560>>2]=((i&255)<<24|v>>>8)&-16777216|((i&16777215)<<8|v>>>24)&16711680|(i>>>8&65280|i>>>24)|l;f[h+564>>2]=o;w=f[h+1048>>2];i=w<<24;j=w<<8;o=0;p=j&16711680|i;n=f[h+1052>>2];j=n<<24|w>>>8;i=n<<8|w>>>24;p=j&65280|i&255|p;i=n;f[h+568>>2]=((i&255)<<24|w>>>8)&-16777216|((i&16777215)<<8|w>>>24)&16711680|(i>>>8&65280|i>>>24)|o;f[h+572>>2]=p;x=f[h+1056>>2];i=x<<24;j=x<<8;p=0;q=j&16711680|i;l=f[h+1060>>2];j=l<<24|x>>>8;i=l<<8|x>>>24;q=j&65280|i&255|q;i=l;f[h+576>>2]=((i&255)<<24|x>>>8)&-16777216|((i&16777215)<<8|x>>>24)&16711680|(i>>>8&65280|i>>>24)|p;f[h+580>>2]=q;y=f[h+1064>>2];i=y<<24;j=y<<8;q=0;D=j&16711680|i;o=f[h+1068>>2];j=o<<24|y>>>8;i=o<<8|y>>>24;D=j&65280|i&255|D;i=o;f[h+584>>2]=((i&255)<<24|y>>>8)&-16777216|((i&16777215)<<8|y>>>24)&16711680|(i>>>8&65280|i>>>24)|q;f[h+588>>2]=D;B=f[h+1072>>2];i=B<<24;j=B<<8;D=0;r=j&16711680|i;p=f[h+1076>>2];j=p<<24|B>>>8;i=p<<8|B>>>24;j=j&65280|i&255|r;i=p;f[h+592>>2]=((i&255)<<24|B>>>8)&-16777216|((i&16777215)<<8|B>>>24)&16711680|(i>>>8&65280|i>>>24)|D;f[h+596>>2]=j;r=f[h+1080>>2];_=r<<24|r<<8&16711680;q=f[h+1084>>2];j=q<<24|r>>>8;i=q<<8|r>>>24;_=j&65280|i&255|_;i=q;f[h+600>>2]=((i&255)<<24|r>>>8)&-16777216|((i&16777215)<<8|r>>>24)&16711680|(i>>>8&65280|i>>>24)|D;f[h+604>>2]=_;E=r^E;S=i^S;F=B^F;T=p^T;G=y^G;U=o^U;H=x^H;V=l^V;I=w^I;W=n^W;J=v^J;X=k^X;K=t^K;Y=Y^L;C=C^s;Z=m^Z;b=b+1|0;if((b|0)!=15e3){continue}break}b=Z;d[h+419|0]=b;d[h+418|0]=b>>>8;d[h+417|0]=b>>>16;d[h+416|0]=b>>>24;d[h+431|0]=K;b=Y;i=K;d[h+430|0]=(b&255)<<24|i>>>8;d[h+429|0]=(b&65535)<<16|i>>>16;d[h+428|0]=(b&16777215)<<8|i>>>24;d[h+427|0]=b;d[h+426|0]=b>>>8;d[h+425|0]=b>>>16;d[h+424|0]=b>>>24;d[h+439|0]=J;b=X;i=J;d[h+438|0]=(b&255)<<24|i>>>8;d[h+437|0]=(b&65535)<<16|i>>>16;d[h+436|0]=(b&16777215)<<8|i>>>24;f[h+420>>2]=C<<8&16711680|C<<24|(C>>>8&65280|C>>>24);d[h+434|0]=b>>>8;d[h+435|0]=b;d[h+433|0]=b>>>16;d[h+432|0]=b>>>24;d[h+447|0]=I;b=W;i=I;d[h+446|0]=(b&255)<<24|i>>>8;d[h+445|0]=(b&65535)<<16|i>>>16;d[h+444|0]=(b&16777215)<<8|i>>>24;d[h+443|0]=b;d[h+442|0]=b>>>8;d[h+441|0]=b>>>16;d[h+440|0]=b>>>24;d[h+455|0]=H;b=V;i=H;d[h+454|0]=(b&255)<<24|i>>>8;d[h+453|0]=(b&65535)<<16|i>>>16;d[h+452|0]=(b&16777215)<<8|i>>>24;d[h+451|0]=b;d[h+449|0]=b>>>16;d[h+450|0]=b>>>8;d[h+448|0]=b>>>24;d[h+463|0]=G;b=U;i=G;d[h+462|0]=(b&255)<<24|i>>>8;d[h+461|0]=(b&65535)<<16|i>>>16;d[h+460|0]=(b&16777215)<<8|i>>>24;d[h+459|0]=b;d[h+458|0]=b>>>8;d[h+457|0]=b>>>16;d[h+456|0]=b>>>24;d[h+471|0]=F;b=T;i=F;d[h+470|0]=(b&255)<<24|i>>>8;d[h+469|0]=(b&65535)<<16|i>>>16;d[h+468|0]=(b&16777215)<<8|i>>>24;d[h+467|0]=b;d[h+466|0]=b>>>8;d[h+464|0]=b>>>24;d[h+465|0]=b>>>16;d[h+479|0]=E;b=S;i=E;d[h+478|0]=(b&255)<<24|i>>>8;d[h+477|0]=(b&65535)<<16|i>>>16;d[h+476|0]=(b&16777215)<<8|i>>>24;d[h+475|0]=b;d[h+474|0]=b>>>8;d[h+473|0]=b>>>16;d[h+472|0]=b>>>24;i=(a<<6)+ -64|0;b=40-i|0;P(e+i|0,h+416|0,b>>>0<64?b:64);b=(a|0)!=1;a=a+1|0;if(b){continue}break}A=h+1104|0;return}z(29717,29667,363,29729);u()}function ra(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0;m=a;u=f[c>>2];r=f[b+8>>2];g=hb(r,0,1073741823);h=B;q=h;A=f[b+12>>2];p=hb(A,0,1073741823);x=B;w=f[b+16>>2];j=hb(w,0,1073741823);s=B;d=s+x|0;e=j;k=e+p|0;if(k>>>0<e>>>0){d=d+1|0}y=f[b+28>>2];e=hb(y,0,103979646);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;C=f[b+24>>2];e=hb(C,0,25712450);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;v=f[b+20>>2];e=hb(v,0,1073736481);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;e=k;k=e+g|0;d=d+h|0;d=k>>>0<e>>>0?d+1|0:d;i=f[b+4>>2];e=hb(i,0,1048575);h=e+k|0;d=B+d|0;d=h>>>0<e>>>0?d+1|0:d;e=h;z=f[b+32>>2];h=hb(z,0,913544844);k=e+h|0;e=B+d|0;o=k;k=k>>>0<h>>>0?e+1|0:e;n=hb(i,0,1073741823);h=B;l=h;d=q+x|0;e=g;i=e+p|0;if(i>>>0<e>>>0){d=d+1|0}e=hb(y,0,913544844);i=e+i|0;d=B+d|0;d=i>>>0<e>>>0?d+1|0:d;e=hb(C,0,103979646);i=e+i|0;d=B+d|0;d=i>>>0<e>>>0?d+1|0:d;e=hb(v,0,25712450);i=e+i|0;d=B+d|0;d=i>>>0<e>>>0?d+1|0:d;e=i;i=hb(w,0,1073736481);t=e+i|0;e=B+d|0;e=t>>>0<i>>>0?e+1|0:e;i=t;t=i+n|0;d=e+h|0;h=f[b>>2];b=hb(h,0,1048575);e=b+t|0;d=B+(t>>>0<i>>>0?d+1|0:d)|0;d=e>>>0<b>>>0?d+1|0:d;b=hb(z,0,170660635);e=b+e|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=d;t=e;d=l+q|0;e=g+n|0;if(e>>>0<g>>>0){d=d+1|0}g=hb(y,0,170660635);i=g+e|0;e=B+d|0;e=i>>>0<g>>>0?e+1|0:e;g=hb(C,0,913544844);i=g+i|0;d=B+e|0;d=i>>>0<g>>>0?d+1|0:d;e=hb(v,0,103979646);g=e+i|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=hb(w,0,25712450);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=hb(A,0,1073736481);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=hb(h,0,1073741823);h=e+g|0;e=B+d|0;e=h>>>0<g>>>0?e+1|0:e;g=h;d=e>>>30|0;e=(e&1073741823)<<2|g>>>30;g=t+e|0;d=b+d|0;h=g;d=g>>>0<e>>>0?d+1|0:d;g=d;b=(d&1073741823)<<2|h>>>30;i=b+o|0;d=(d>>>30|0)+k|0;d=i>>>0<b>>>0?d+1|0:d;b=d;k=0;t=k;e=i;D=e<<6&1073741760|((g&16777215)<<8|h>>>24)&63;g=hb(D,k,485872621);d=B;e=u-(g&1073741823)|0;f[m>>2]=(e>>>1&1073741824)+e;k=a;n=f[c+4>>2]+(e>>31)|0;e=d;d=d>>>30|0;e=(e&1073741823)<<2|g>>>30;g=hb(D,t,541690985)+e|0;d=d+B|0;u=g;h=g>>>0<e>>>0?d+1|0:d;l=hb(v,0,1073741823);q=B;d=q+s|0;e=l;g=e+j|0;if(g>>>0<e>>>0){d=d+1|0}e=hb(y,0,25712450);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=hb(C,0,1073736481);m=e+g|0;e=B+d|0;e=m>>>0<g>>>0?e+1|0:e;g=p;m=g+m|0;d=e+x|0;d=m>>>0<g>>>0?d+1|0:d;e=hb(r,0,1048575);g=e+m|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=hb(z,0,103979646);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=d;o=g;g=b;d=g>>>30|0;g=(g&1073741823)<<2|i>>>30;m=o+g|0;d=d+e|0;o=m;g=m>>>0<g>>>0?d+1|0:d;m=0;J=m;e=o;E=e<<6&1073741760|((b&16777215)<<8|i>>>24)&63;b=hb(E,m,485872621);d=b+u|0;e=B+h|0;r=d;h=d>>>0<b>>>0?e+1|0:e;b=n-(d&1073741823)|0;f[k+4>>2]=(b>>>1&1073741824)+b;i=a;n=f[c+8>>2]+(b>>31)|0;b=hb(E,m,541690985);d=B;e=b;b=hb(D,t,796511589);e=e+b|0;d=B+d|0;u=e;m=e>>>0<b>>>0?d+1|0:d;k=hb(C,0,1073741823);x=B;d=x+q|0;b=k;e=b+l|0;if(e>>>0<b>>>0){d=d+1|0}q=e;b=hb(y,0,1073736481);e=e+b|0;l=d;d=d+B|0;d=e>>>0<b>>>0?d+1|0:d;b=e+j|0;d=d+s|0;d=b>>>0<j>>>0?d+1|0:d;e=b;b=hb(A,0,1048575);j=e+b|0;e=B+d|0;e=j>>>0<b>>>0?e+1|0:e;b=hb(z,0,25712450);j=b+j|0;d=B+e|0;e=j;b=e>>>0<b>>>0?d+1|0:d;e=g;d=e>>>30|0;e=(e&1073741823)<<2|o>>>30;j=j+e|0;d=b+d|0;p=j;b=j>>>0<e>>>0?d+1|0:d;j=0;K=j;e=p;F=e<<6&1073741760|((g&16777215)<<8|o>>>24)&63;e=hb(F,j,485872621);g=e+u|0;d=B+m|0;d=g>>>0<e>>>0?d+1|0:d;e=d;o=g;d=h>>>30|0;g=(h&1073741823)<<2|r>>>30;h=o+g|0;e=d+e|0;s=h;h=h>>>0<g>>>0?e+1|0:e;d=n-(s&1073741823)|0;f[i+8>>2]=(d>>>1&1073741824)+d;n=f[c+12>>2]+(d>>31)|0;e=hb(E,J,796511589);d=B;g=e;e=hb(D,t,935229352);g=g+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=hb(F,j,541690985);g=e+g|0;d=B+d|0;u=g;m=g>>>0<e>>>0?d+1|0:d;j=hb(y,0,1073741823);r=B;d=r+l|0;e=j;g=e+q|0;if(g>>>0<e>>>0){d=d+1|0}e=hb(w,0,1048575);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=hb(z,0,1073736481);l=e+g|0;e=B+d|0;e=l>>>0<g>>>0?e+1|0:e;g=b;d=g>>>30|0;g=(g&1073741823)<<2|p>>>30;l=g+l|0;d=d+e|0;o=l;g=l>>>0<g>>>0?d+1|0:d;l=0;H=l;e=o;G=e<<6&1073741760|((b&16777215)<<8|p>>>24)&63;b=hb(G,l,485872621);e=b+u|0;d=B+m|0;d=e>>>0<b>>>0?d+1|0:d;l=e;e=h>>>30|0;b=(h&1073741823)<<2|s>>>30;h=l+b|0;d=d+e|0;w=h;h=h>>>0<b>>>0?d+1|0:d;b=n-(w&1073741823)|0;f[i+12>>2]=(b>>>1&1073741824)+b;l=a;q=f[c+16>>2]+(b>>31)|0;b=hb(E,J,935229352);d=B;e=b;b=hb(D,t,20);e=e+b|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=hb(F,K,796511589);e=b+e|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=hb(G,H,541690985);e=b+e|0;d=B+d|0;n=e;i=e>>>0<b>>>0?d+1|0:d;A=hb(z,0,1073741823);u=B;e=r+u|0;b=j+A|0;if(b>>>0<j>>>0){e=e+1|0}s=b;m=e;b=hb(v,0,1048575)+k|0;d=x+B|0;d=b>>>0<k>>>0?d+1|0:d;j=b+s|0;d=d+e|0;d=j>>>0<b>>>0?d+1|0:d;e=g>>>30|0;b=(g&1073741823)<<2|o>>>30;j=b+j|0;d=d+e|0;p=j;b=j>>>0<b>>>0?d+1|0:d;j=0;I=j;e=p;v=e<<6&1073741760|((g&16777215)<<8|o>>>24)&63;e=hb(v,j,485872621);g=e+n|0;d=B+i|0;d=g>>>0<e>>>0?d+1|0:d;e=d;j=g;d=h>>>30|0;g=(h&1073741823)<<2|w>>>30;h=j+g|0;d=d+e|0;r=h;h=h>>>0<g>>>0?d+1|0:d;n=q-(r&1073741823)|0;f[l+16>>2]=(n>>>1&1073741824)+n;k=a;o=f[c+20>>2];e=hb(F,K,935229352);d=B;a=e;e=hb(E,J,20);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=hb(G,H,796511589);j=a+g|0;e=B+d|0;e=j>>>0<g>>>0?e+1|0:e;g=hb(v,I,541690985);j=g+j|0;d=B+e|0;l=j;j=j>>>0<g>>>0?d+1|0:d;e=hb(C,0,1048575);g=e+s|0;d=B+m|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=b;e=g>>>30|0;g=(g&1073741823)<<2|p>>>30;m=a+g|0;d=d+e|0;d=m>>>0<g>>>0?d+1|0:d;g=d;i=0;w=i;e=m;s=e<<6&1073741760|((b&16777215)<<8|p>>>24)&63;b=hb(s,i,485872621);e=b+l|0;d=B+j|0;d=e>>>0<b>>>0?d+1|0:d;b=d;a=e;d=h>>>30|0;e=(h&1073741823)<<2|r>>>30;h=a+e|0;d=b+d|0;q=h;h=h>>>0<e>>>0?d+1|0:d;n=(o-(q&1073741823)|0)+(n>>31)|0;f[k+20>>2]=(n>>>1&1073741824)+n;o=f[c+24>>2];b=hb(G,H,935229352);d=B;a=b;b=hb(F,K,20);e=a+b|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=hb(v,I,796511589);j=b+e|0;e=B+d|0;e=j>>>0<b>>>0?e+1|0:e;b=hb(s,i,541690985);j=b+j|0;d=B+e|0;p=j;j=j>>>0<b>>>0?d+1|0:d;b=hb(y,0,1048575);e=b+A|0;d=B+u|0;d=e>>>0<b>>>0?d+1|0:d;a=e;e=g>>>30|0;b=(g&1073741823)<<2|m>>>30;i=a+b|0;d=d+e|0;d=i>>>0<b>>>0?d+1|0:d;b=d;l=0;x=l;e=i;r=e<<6&1073741760|((g&16777215)<<8|m>>>24)&63;e=hb(r,l,485872621);g=e+p|0;d=B+j|0;d=g>>>0<e>>>0?d+1|0:d;e=d;a=g;d=h>>>30|0;g=(h&1073741823)<<2|q>>>30;h=a+g|0;d=d+e|0;q=h;h=h>>>0<g>>>0?d+1|0:d;n=(o-(q&1073741823)|0)+(n>>31)|0;f[k+24>>2]=(n>>>1&1073741824)+n;j=k;u=f[c+28>>2];e=hb(v,I,935229352);d=B;a=e;e=hb(G,H,20);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=hb(s,w,796511589);k=a+g|0;e=B+d|0;e=k>>>0<g>>>0?e+1|0:e;g=hb(r,l,541690985);k=g+k|0;d=B+e|0;o=k;e=k>>>0<g>>>0?d+1|0:d;g=b;d=g>>>30|0;g=(g&1073741823)<<2|i>>>30;k=hb(z,0,1048575)+g|0;d=d+B|0;m=k;g=k>>>0<g>>>0?d+1|0:d;p=0;l=p;k=k<<6&1073741760|((b&16777215)<<8|i>>>24)&63;b=hb(k,l,485872621);i=b+o|0;d=B+e|0;e=i;b=e>>>0<b>>>0?d+1|0:d;d=h>>>30|0;h=(h&1073741823)<<2|q>>>30;i=h+e|0;e=b+d|0;o=i;b=i>>>0<h>>>0?e+1|0:e;p=(u-(i&1073741823)|0)+(n>>31)|0;f[j+28>>2]=(p>>>1&1073741824)+p;h=j;j=f[c+32>>2];i=D;d=t<<12|i>>>20;c=i<<12;i=hb(v,I,20)+c|0;d=d+B|0;d=i>>>0<c>>>0?d+1|0:d;c=hb(s,w,935229352);e=c+i|0;d=B+d|0;d=e>>>0<c>>>0?d+1|0:d;a=e;e=hb((g&16777215)<<8|m>>>24,g>>>24|0,485872621);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;c=hb(r,x,796511589);g=c+g|0;e=B+d|0;e=g>>>0<c>>>0?e+1|0:e;c=hb(k,l,541690985);g=c+g|0;e=g;e=((b&1073741823)<<2|o>>>30)+e|0;b=(j-(e&16777215)|0)+(p>>31)|0;f[h+32>>2]=(b>>>7&16777216)+b;oa(h);oa(h)}function M(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0,R=0,S=0;t=f[b+4>>2];h=f[c+4>>2];O=h<<1;d=hb(t,0,O);j=B;u=f[b>>2];g=f[c+8>>2];H=g;o=hb(u,0,g);e=o+d|0;d=B+j|0;d=e>>>0<o>>>0?d+1|0:d;v=f[b+8>>2];w=f[c>>2];j=hb(v,0,w);e=j+e|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;x=f[b+12>>2];Q=f[c+36>>2];M=l(Q,38);j=hb(x,0,M);e=j+e|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;y=f[b+16>>2];K=f[c+32>>2];s=l(K,19);o=hb(y,0,s);j=o+e|0;e=B+d|0;e=j>>>0<o>>>0?e+1|0:e;z=f[b+20>>2];p=f[c+28>>2];L=p&2147483647;n=l(L,38);o=hb(z,0,n);j=o+j|0;d=B+e|0;d=j>>>0<o>>>0?d+1|0:d;e=j;A=f[b+24>>2];m=f[c+24>>2];G=l(m,19);j=hb(A,0,G);e=e+j|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;C=f[b+28>>2];o=f[c+20>>2];q=o&2147483647;R=l(q,38);i=hb(C,0,R);j=i+e|0;e=B+d|0;e=j>>>0<i>>>0?e+1|0:e;D=f[b+32>>2];P=f[c+16>>2];r=l(P,19);i=hb(D,0,r);j=i+j|0;d=B+e|0;d=j>>>0<i>>>0?d+1|0:d;E=f[b+36>>2];i=f[c+12>>2];F=i&2147483647;I=l(F,38);c=hb(E,0,I);b=c+j|0;d=B+d|0;k=b;c=b>>>0<c>>>0?d+1|0:d;b=hb(t,0,w);d=B;j=h;J=hb(u,0,j);b=J+b|0;e=B+d|0;e=b>>>0<J>>>0?e+1|0:e;J=l(Q,19);h=hb(v,0,J);b=h+b|0;d=B+e|0;d=b>>>0<h>>>0?d+1|0:d;e=hb(s,0,x);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;L=l(L,19);h=hb(y,0,L);e=h+b|0;b=B+d|0;b=e>>>0<h>>>0?b+1|0:b;h=hb(z,0,G);e=h+e|0;d=B+b|0;d=e>>>0<h>>>0?d+1|0:d;q=l(q,19);h=hb(A,0,q);b=h+e|0;e=B+d|0;e=b>>>0<h>>>0?e+1|0:e;h=hb(C,0,r);b=h+b|0;d=B+e|0;d=b>>>0<h>>>0?d+1|0:d;e=hb(D,0,l(F,19));b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;F=l(g,19);g=hb(E,0,F);e=g+b|0;b=B+d|0;d=e;g=d>>>0<g>>>0?b+1|0:b;b=hb(t,0,M);e=B;N=k;S=d;k=hb(u,0,w);b=k+b|0;d=B+e|0;d=b>>>0<k>>>0?d+1|0:d;k=hb(s,0,v);b=k+b|0;e=B+d|0;e=b>>>0<k>>>0?e+1|0:e;k=hb(x,0,n);b=k+b|0;d=B+e|0;d=b>>>0<k>>>0?d+1|0:d;e=hb(y,0,G);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=hb(z,0,R);e=k+b|0;b=B+d|0;b=e>>>0<k>>>0?b+1|0:b;k=hb(A,0,r);e=k+e|0;d=B+b|0;I=hb(C,0,I);b=I+e|0;e=B+(e>>>0<k>>>0?d+1|0:d)|0;e=b>>>0<I>>>0?e+1|0:e;F=hb(D,0,F);b=F+b|0;d=B+e|0;d=b>>>0<F>>>0?d+1|0:d;e=hb(E,0,l(j,38));b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;F=b;e=d>>>26|0;h=(d&67108863)<<6|b>>>26;d=S+h|0;b=e+g|0;I=d;e=d;b=d>>>0<h>>>0?b+1|0:b;d=b>>>25|0;e=(b&33554431)<<7|e>>>25;b=N+e|0;d=c+d|0;g=b;c=b>>>0<e>>>0?d+1|0:d;f[a+8>>2]=b&67108863;h=a;b=hb(t,0,H);d=B;N=i;e=hb(u,0,i);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=hb(v,0,j);b=k+b|0;e=B+d|0;e=b>>>0<k>>>0?e+1|0:e;k=hb(w,0,x);d=k+b|0;b=B+e|0;b=d>>>0<k>>>0?b+1|0:b;k=hb(y,0,J);e=k+d|0;d=B+b|0;d=e>>>0<k>>>0?d+1|0:d;b=e;e=hb(s,0,z);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(A,0,L);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=hb(G,0,C);b=k+b|0;e=B+d|0;q=hb(D,0,q);d=q+b|0;b=B+(b>>>0<k>>>0?e+1|0:e)|0;r=hb(r,0,E);e=r+d|0;d=B+(d>>>0<q>>>0?b+1|0:b)|0;d=e>>>0<r>>>0?d+1|0:d;b=e;e=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=b+c|0;d=d+e|0;r=b;c=b>>>0<c>>>0?d+1|0:d;f[h+12>>2]=b&33554431;g=h;q=i<<1;b=hb(t,0,q);d=B;h=P;e=hb(u,0,h);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;i=hb(v,0,H);e=i+b|0;b=B+d|0;b=e>>>0<i>>>0?b+1|0:b;i=hb(x,0,O);e=i+e|0;d=B+b|0;d=e>>>0<i>>>0?d+1|0:d;i=hb(w,0,y);b=i+e|0;e=B+d|0;e=b>>>0<i>>>0?e+1|0:e;i=hb(z,0,M);b=i+b|0;d=B+e|0;d=b>>>0<i>>>0?d+1|0:d;e=hb(s,0,A);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;i=hb(C,0,n);e=i+b|0;b=B+d|0;b=e>>>0<i>>>0?b+1|0:b;i=hb(G,0,D);e=i+e|0;d=B+b|0;d=e>>>0<i>>>0?d+1|0:d;i=hb(E,0,R);b=i+e|0;e=B+d|0;e=b>>>0<i>>>0?e+1|0:e;d=b;b=c>>>25|0;i=(c&33554431)<<7|r>>>25;c=d+i|0;d=b+e|0;r=c;c=c>>>0<i>>>0?d+1|0:d;f[g+16>>2]=r&67108863;b=hb(t,0,h);d=B;i=o;e=hb(u,0,i);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(v,0,N);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;g=hb(x,0,H);b=g+b|0;e=B+d|0;e=b>>>0<g>>>0?e+1|0:e;g=hb(y,0,j);d=g+b|0;b=B+e|0;b=d>>>0<g>>>0?b+1|0:b;g=hb(w,0,z);e=g+d|0;d=B+b|0;d=e>>>0<g>>>0?d+1|0:d;b=e;e=hb(A,0,J);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(s,0,C);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;g=hb(D,0,L);b=g+b|0;e=B+d|0;e=b>>>0<g>>>0?e+1|0:e;g=hb(G,0,E);d=g+b|0;b=B+e|0;b=d>>>0<g>>>0?b+1|0:b;g=d;d=c>>>26|0;e=(c&67108863)<<6|r>>>26;c=g+e|0;d=b+d|0;g=c;c=c>>>0<e>>>0?d+1|0:d;f[a+20>>2]=g&33554431;G=i<<1;b=hb(t,0,G);d=B;o=m;e=hb(u,0,m);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;m=hb(v,0,h);b=m+b|0;e=B+d|0;e=b>>>0<m>>>0?e+1|0:e;m=hb(x,0,q);d=m+b|0;b=B+e|0;b=d>>>0<m>>>0?b+1|0:b;m=hb(y,0,H);e=m+d|0;d=B+b|0;d=e>>>0<m>>>0?d+1|0:d;b=e;e=hb(z,0,O);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(w,0,A);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;m=hb(C,0,M);b=m+b|0;e=B+d|0;e=b>>>0<m>>>0?e+1|0:e;m=hb(s,0,D);d=m+b|0;b=B+e|0;b=d>>>0<m>>>0?b+1|0:b;m=hb(E,0,n);e=m+d|0;d=B+b|0;d=e>>>0<m>>>0?d+1|0:d;b=e;e=c>>>25|0;c=(c&33554431)<<7|g>>>25;b=b+c|0;d=d+e|0;g=b;c=b>>>0<c>>>0?d+1|0:d;f[a+24>>2]=b&67108863;m=a;b=hb(t,0,o);d=B;P=p;e=hb(u,0,p);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;n=hb(v,0,i);e=n+b|0;b=B+d|0;b=e>>>0<n>>>0?b+1|0:b;n=hb(x,0,h);e=n+e|0;d=B+b|0;d=e>>>0<n>>>0?d+1|0:d;n=hb(y,0,N);b=n+e|0;e=B+d|0;e=b>>>0<n>>>0?e+1|0:e;n=hb(z,0,H);b=n+b|0;d=B+e|0;d=b>>>0<n>>>0?d+1|0:d;e=hb(A,0,j);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;n=hb(w,0,C);e=n+b|0;b=B+d|0;b=e>>>0<n>>>0?b+1|0:b;n=hb(D,0,J);e=n+e|0;d=B+b|0;s=hb(s,0,E);b=s+e|0;e=B+(e>>>0<n>>>0?d+1|0:d)|0;e=b>>>0<s>>>0?e+1|0:e;a=b;b=c>>>26|0;g=(c&67108863)<<6|g>>>26;c=a+g|0;d=b+e|0;d=c>>>0<g>>>0?d+1|0:d;g=c;c=d;f[m+28>>2]=g&33554431;b=hb(t,0,p<<1);d=B;e=hb(u,0,K);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(v,0,o);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;p=hb(x,0,G);b=p+b|0;e=B+d|0;e=b>>>0<p>>>0?e+1|0:e;p=hb(y,0,h);d=p+b|0;b=B+e|0;b=d>>>0<p>>>0?b+1|0:b;p=hb(z,0,q);e=p+d|0;d=B+b|0;d=e>>>0<p>>>0?d+1|0:d;a=e;e=hb(A,0,H);b=a+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(C,0,O);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;p=hb(w,0,D);b=p+b|0;e=B+d|0;e=b>>>0<p>>>0?e+1|0:e;p=hb(E,0,M);d=p+b|0;b=B+e|0;b=d>>>0<p>>>0?b+1|0:b;a=d;d=c>>>25|0;e=(c&33554431)<<7|g>>>25;c=a+e|0;d=b+d|0;g=c;c=c>>>0<e>>>0?d+1|0:d;f[m+32>>2]=g&67108863;b=hb(t,0,K);d=B;e=hb(u,0,Q);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;K=hb(v,0,P);b=K+b|0;e=B+d|0;o=hb(x,0,o);d=o+b|0;b=B+(b>>>0<K>>>0?e+1|0:e)|0;b=d>>>0<o>>>0?b+1|0:b;o=hb(y,0,i);e=o+d|0;d=B+b|0;d=e>>>0<o>>>0?d+1|0:d;a=e;e=hb(z,0,h);b=a+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(A,0,N);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;h=hb(C,0,H);b=h+b|0;e=B+d|0;e=b>>>0<h>>>0?e+1|0:e;h=hb(D,0,j);d=h+b|0;b=B+e|0;b=d>>>0<h>>>0?b+1|0:b;h=hb(w,0,E);e=h+d|0;d=B+b|0;d=e>>>0<h>>>0?d+1|0:d;a=e;e=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=a+c|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;f[m+36>>2]=b&33554431;c=F&67108863;b=hb((d&33554431)<<7|b>>>25,0,19)+c|0;d=B;d=b>>>0<c>>>0?d+1|0:d;f[m>>2]=b&67108863;f[m+4>>2]=(I&33554431)+((d&67108863)<<6|b>>>26)}function T(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0;if((c|0)>0){while(1){d=g<<3;j=d+a|0;h=j;e=f[h>>2];d=b+d|0;i=f[d>>2];d=f[h+4>>2]^f[d+4>>2];f[h>>2]=e^i;f[h+4>>2]=d;g=g+1|0;if((g|0)!=(c|0)){continue}break}}e=A-48|0;N=f[a+80>>2];P=f[a+84>>2];l=f[a+160>>2];m=f[a+164>>2];O=f[a+120>>2];Q=f[a+124>>2];b=f[a+88>>2];j=f[a+92>>2];i=f[a+192>>2];k=f[a+196>>2];s=f[a+152>>2];G=f[a+156>>2];y=f[a+112>>2];H=f[a+116>>2];n=f[a+184>>2];r=f[a+188>>2];z=f[a+144>>2];I=f[a+148>>2];C=f[a+104>>2];J=f[a+108>>2];t=f[a+176>>2];K=f[a+180>>2];D=f[a+136>>2];L=f[a+140>>2];E=f[a+96>>2];u=f[a+100>>2];v=f[a+168>>2];w=f[a+172>>2];h=f[a+128>>2];g=f[a+132>>2];d=f[a>>2];p=f[a+4>>2];c=0;while(1){x=f[a+44>>2];M=m^(Q^(P^(p^x)));W=M;R=f[a+40>>2];S=l^(O^(N^(d^R)));f[e>>2]=S;f[e+4>>2]=M;M=f[a+52>>2];T=f[a+12>>2];o=w^(g^(j^(M^T)));w=o;U=f[a+48>>2];V=f[a+8>>2];v=v^(h^(b^(U^V)));f[e+8>>2]=v;f[e+12>>2]=o;q=L;o=u;L=f[a+60>>2];u=f[a+20>>2];o=K^(q^(o^(L^u)));K=o;F=D;q=E;D=f[a+56>>2];E=f[a+16>>2];t=t^(F^(q^(D^E)));f[e+16>>2]=t;f[e+20>>2]=o;q=I;o=J;I=f[a+68>>2];J=f[a+28>>2];o=r^(q^(o^(I^J)));r=o;F=z;q=C;z=f[a+64>>2];C=f[a+24>>2];n=n^(F^(q^(z^C)));f[e+24>>2]=n;f[e+28>>2]=o;q=H;H=f[a+76>>2];o=f[a+36>>2];k=k^(G^(q^(H^o)));G=k;q=y;y=f[a+72>>2];F=f[a+32>>2];s=i^(s^(q^(y^F)));f[e+32>>2]=s;f[e+36>>2]=k;k=h;h=jb(t,K,1)^S;f[a+128>>2]=k^h;k=g;g=W^B;f[a+132>>2]=k^g;f[a+88>>2]=b^h;f[a+92>>2]=g^j;f[a+48>>2]=h^U;f[a+52>>2]=g^M;i=g^T;j=i;b=h^V;f[a+8>>2]=b;f[a+12>>2]=i;i=jb(n,r,1)^v;f[a+56>>2]=i^D;k=w^B;f[a+60>>2]=k^L;f[a+16>>2]=i^E;f[a+20>>2]=k^u;u=l;l=jb(v,w,1)^s;f[a+160>>2]=u^l;w=m;m=G^B;f[a+164>>2]=w^m;f[a+120>>2]=l^O;f[a+124>>2]=m^Q;f[a+80>>2]=l^N;f[a+84>>2]=m^P;f[a+40>>2]=l^R;f[a+44>>2]=m^x;f[a>>2]=d^l;f[a+4>>2]=m^p;d=f[a+172>>2]^g;f[a+168>>2]=f[a+168>>2]^h;f[a+172>>2]=d;d=f[a+100>>2]^k;f[a+96>>2]=f[a+96>>2]^i;f[a+100>>2]=d;d=f[a+140>>2]^k;f[a+136>>2]=f[a+136>>2]^i;f[a+140>>2]=d;d=f[a+180>>2]^k;f[a+176>>2]=f[a+176>>2]^i;f[a+180>>2]=d;d=jb(s,G,1)^t;f[a+64>>2]=d^z;h=K^B;f[a+68>>2]=h^I;f[a+24>>2]=d^C;f[a+28>>2]=h^J;g=f[a+108>>2]^h;f[a+104>>2]=f[a+104>>2]^d;f[a+108>>2]=g;g=f[a+148>>2]^h;f[a+144>>2]=f[a+144>>2]^d;f[a+148>>2]=g;h=f[a+188>>2]^h;f[a+184>>2]=f[a+184>>2]^d;f[a+188>>2]=h;d=jb(f[e>>2],f[e+4>>2],1)^n;f[a+72>>2]=d^y;h=r^B;f[a+76>>2]=h^H;f[a+32>>2]=d^F;f[a+36>>2]=h^o;g=f[a+116>>2]^h;f[a+112>>2]=f[a+112>>2]^d;f[a+116>>2]=g;g=f[a+156>>2]^h;f[a+152>>2]=f[a+152>>2]^d;f[a+156>>2]=g;h=f[a+196>>2]^h;f[a+192>>2]=f[a+192>>2]^d;f[a+196>>2]=h;g=0;while(1){k=g<<2;i=(f[k+30384>>2]<<3)+a|0;h=i;d=f[h>>2];h=f[h+4>>2];f[i>>2]=jb(b,j,f[k+30480>>2]);f[i+4>>2]=B;b=d;j=h;g=g+1|0;if((g|0)!=24){continue}break}b=f[a+36>>2];f[e+32>>2]=f[a+32>>2];f[e+36>>2]=b;b=f[a+28>>2];f[e+24>>2]=f[a+24>>2];f[e+28>>2]=b;d=f[a+20>>2];b=d;j=f[a+16>>2];f[e+16>>2]=j;f[e+20>>2]=d;d=f[a+4>>2];f[e>>2]=f[a>>2];f[e+4>>2]=d;g=f[a+12>>2];d=g;h=f[a+8>>2];f[e+8>>2]=h;f[e+12>>2]=d;g=f[a+4>>2]^(d^-1)&b;f[a>>2]=f[a>>2]^(h^-1)&j;f[a+4>>2]=g;g=f[e+28>>2];b=f[a+12>>2]^g&(b^-1);i=f[e+24>>2];f[a+8>>2]=f[a+8>>2]^i&(j^-1);f[a+12>>2]=b;b=f[e+36>>2];j=f[a+20>>2]^b&(g^-1);g=f[e+32>>2];f[a+16>>2]=f[a+16>>2]^g&(i^-1);f[a+20>>2]=j;j=f[e+4>>2];b=f[a+28>>2]^j&(b^-1);i=f[e>>2];f[a+24>>2]=f[a+24>>2]^i&(g^-1);f[a+28>>2]=b;b=f[a+36>>2]^(j^-1)&d;f[a+32>>2]=f[a+32>>2]^(i^-1)&h;f[a+36>>2]=b;b=f[a+76>>2];f[e+32>>2]=f[a+72>>2];f[e+36>>2]=b;b=f[a+68>>2];f[e+24>>2]=f[a+64>>2];f[e+28>>2]=b;d=f[a+60>>2];b=d;j=f[a+56>>2];f[e+16>>2]=j;f[e+20>>2]=d;g=f[a+52>>2];d=g;h=f[a+48>>2];f[e+8>>2]=h;f[e+12>>2]=d;g=f[a+44>>2];f[e>>2]=f[a+40>>2];f[e+4>>2]=g;g=f[a+44>>2]^(d^-1)&b;f[a+40>>2]=f[a+40>>2]^(h^-1)&j;f[a+44>>2]=g;g=f[e+28>>2];b=f[a+52>>2]^g&(b^-1);i=f[e+24>>2];f[a+48>>2]=f[a+48>>2]^i&(j^-1);f[a+52>>2]=b;b=f[e+36>>2];j=f[a+60>>2]^b&(g^-1);g=f[e+32>>2];f[a+56>>2]=f[a+56>>2]^g&(i^-1);f[a+60>>2]=j;j=f[e+4>>2];b=f[a+68>>2]^j&(b^-1);i=f[e>>2];f[a+64>>2]=f[a+64>>2]^i&(g^-1);f[a+68>>2]=b;b=f[a+76>>2]^(j^-1)&d;f[a+72>>2]=f[a+72>>2]^(i^-1)&h;f[a+76>>2]=b;b=f[a+116>>2];f[e+32>>2]=f[a+112>>2];f[e+36>>2]=b;b=f[a+108>>2];f[e+24>>2]=f[a+104>>2];f[e+28>>2]=b;j=f[a+100>>2];b=j;d=f[a+96>>2];f[e+16>>2]=d;f[e+20>>2]=b;j=f[a+92>>2];h=j;g=f[a+88>>2];f[e+8>>2]=g;f[e+12>>2]=h;j=f[a+84>>2];f[e>>2]=f[a+80>>2];f[e+4>>2]=j;j=f[a+84>>2]^(h^-1)&b;P=j;N=f[a+80>>2]^(g^-1)&d;f[a+80>>2]=N;f[a+84>>2]=j;i=f[e+28>>2];k=f[a+92>>2]^i&(b^-1);j=k;l=f[e+24>>2];b=f[a+88>>2]^l&(d^-1);f[a+88>>2]=b;f[a+92>>2]=j;d=f[e+36>>2];i=f[a+100>>2]^d&(i^-1);u=i;k=f[e+32>>2];E=f[a+96>>2]^k&(l^-1);f[a+96>>2]=E;f[a+100>>2]=i;i=f[e+4>>2];d=f[a+108>>2]^i&(d^-1);J=d;l=f[e>>2];C=f[a+104>>2]^l&(k^-1);f[a+104>>2]=C;f[a+108>>2]=d;d=f[a+116>>2]^(i^-1)&h;H=d;y=f[a+112>>2]^(l^-1)&g;f[a+112>>2]=y;f[a+116>>2]=d;d=f[a+156>>2];f[e+32>>2]=f[a+152>>2];f[e+36>>2]=d;d=f[a+148>>2];f[e+24>>2]=f[a+144>>2];f[e+28>>2]=d;g=f[a+140>>2];d=g;h=f[a+136>>2];f[e+16>>2]=h;f[e+20>>2]=d;g=f[a+132>>2];i=g;k=f[a+128>>2];f[e+8>>2]=k;f[e+12>>2]=g;g=f[a+124>>2];f[e>>2]=f[a+120>>2];f[e+4>>2]=g;g=f[a+124>>2]^(i^-1)&d;Q=g;O=f[a+120>>2]^(k^-1)&h;f[a+120>>2]=O;f[a+124>>2]=g;l=f[e+28>>2];d=f[a+132>>2]^l&(d^-1);g=d;m=f[e+24>>2];h=f[a+128>>2]^m&(h^-1);f[a+128>>2]=h;f[a+132>>2]=d;d=f[e+36>>2];l=f[a+140>>2]^d&(l^-1);L=l;p=f[e+32>>2];D=f[a+136>>2]^p&(m^-1);f[a+136>>2]=D;f[a+140>>2]=l;l=f[e+4>>2];d=f[a+148>>2]^l&(d^-1);I=d;m=f[e>>2];z=f[a+144>>2]^m&(p^-1);f[a+144>>2]=z;f[a+148>>2]=d;d=f[a+156>>2]^(l^-1)&i;G=d;s=f[a+152>>2]^(m^-1)&k;f[a+152>>2]=s;f[a+156>>2]=d;d=f[a+196>>2];f[e+32>>2]=f[a+192>>2];f[e+36>>2]=d;d=f[a+188>>2];f[e+24>>2]=f[a+184>>2];f[e+28>>2]=d;k=f[a+180>>2];d=k;i=f[a+176>>2];f[e+16>>2]=i;f[e+20>>2]=d;l=f[a+172>>2];k=l;p=f[a+168>>2];f[e+8>>2]=p;f[e+12>>2]=l;l=f[a+164>>2];f[e>>2]=f[a+160>>2];f[e+4>>2]=l;n=f[a+164>>2]^(k^-1)&d;m=n;l=f[a+160>>2]^(p^-1)&i;f[a+160>>2]=l;f[a+164>>2]=m;n=f[e+28>>2];d=f[a+172>>2]^n&(d^-1);w=d;r=f[e+24>>2];v=f[a+168>>2]^r&(i^-1);f[a+168>>2]=v;f[a+172>>2]=d;d=f[e+36>>2];i=f[a+180>>2]^d&(n^-1);K=i;n=f[e+32>>2];t=f[a+176>>2]^n&(r^-1);f[a+176>>2]=t;f[a+180>>2]=i;i=f[e+4>>2];d=f[a+188>>2]^i&(d^-1);r=d;x=f[e>>2];n=f[a+184>>2]^x&(n^-1);f[a+184>>2]=n;f[a+188>>2]=d;d=f[a+196>>2]^(i^-1)&k;k=d;i=f[a+192>>2]^(x^-1)&p;f[a+192>>2]=i;f[a+196>>2]=d;p=(c<<3)+30576|0;d=f[a>>2]^f[p>>2];x=f[a+4>>2]^f[p+4>>2];p=x;f[a>>2]=d;f[a+4>>2]=p;c=c+1|0;if((c|0)!=24){continue}break}}function Sa(a,b,c,i,j,k){var l=0,m=0,n=0;l=A-288|0;A=l;a:{if(c){O(l- -64|0,0,131);m=h[529]|h[530]<<16;n=h[527]|h[528]<<16;e[l+270>>1]=n;e[l+272>>1]=n>>>16;e[l+274>>1]=m;e[l+276>>1]=m>>>16;m=f[263];f[l+264>>2]=f[262];f[l+268>>2]=m;m=f[261];f[l+256>>2]=f[260];f[l+260>>2]=m;X(b,c,l+256|0,l+208|0);Z(l- -64|0,l+208|0,l+240|0);f[l+232>>2]=0;f[l+236>>2]=0;f[l+224>>2]=0;f[l+228>>2]=0;f[l+240>>2]=0;f[l+244>>2]=0;f[l+216>>2]=0;f[l+220>>2]=0;f[l+208>>2]=0;f[l+212>>2]=0;Y(l,l- -64|0,a,64);O(l- -64|0,0,131);break a}b=a;c=g[b+60|0]|g[b+61|0]<<8|(g[b+62|0]<<16|g[b+63|0]<<24);f[l+56>>2]=g[b+56|0]|g[b+57|0]<<8|(g[b+58|0]<<16|g[b+59|0]<<24);f[l+60>>2]=c;c=g[b+52|0]|g[b+53|0]<<8|(g[b+54|0]<<16|g[b+55|0]<<24);f[l+48>>2]=g[b+48|0]|g[b+49|0]<<8|(g[b+50|0]<<16|g[b+51|0]<<24);f[l+52>>2]=c;c=g[b+44|0]|g[b+45|0]<<8|(g[b+46|0]<<16|g[b+47|0]<<24);f[l+40>>2]=g[b+40|0]|g[b+41|0]<<8|(g[b+42|0]<<16|g[b+43|0]<<24);f[l+44>>2]=c;c=g[b+36|0]|g[b+37|0]<<8|(g[b+38|0]<<16|g[b+39|0]<<24);f[l+32>>2]=g[b+32|0]|g[b+33|0]<<8|(g[b+34|0]<<16|g[b+35|0]<<24);f[l+36>>2]=c;c=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);f[l+24>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);f[l+28>>2]=c;c=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[l+16>>2]=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);f[l+20>>2]=c;c=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[l>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[l+4>>2]=c;c=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[l+8>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[l+12>>2]=c}b:{if(j){O(l- -64|0,0,131);b=h[529]|h[530]<<16;c=h[527]|h[528]<<16;e[l+270>>1]=c;e[l+272>>1]=c>>>16;e[l+274>>1]=b;e[l+276>>1]=b>>>16;b=f[263];f[l+264>>2]=f[262];f[l+268>>2]=b;b=f[261];f[l+256>>2]=f[260];f[l+260>>2]=b;X(i,j,l+256|0,l+208|0);Z(l- -64|0,l+208|0,l+240|0);f[l+232>>2]=0;f[l+236>>2]=0;f[l+224>>2]=0;f[l+228>>2]=0;f[l+240>>2]=0;f[l+244>>2]=0;f[l+216>>2]=0;f[l+220>>2]=0;f[l+208>>2]=0;f[l+212>>2]=0;Y(k,l- -64|0,l,64);O(l- -64|0,0,131);break b}b=f[l+4>>2];c=f[l>>2];d[k|0]=c;d[k+1|0]=c>>>8;d[k+2|0]=c>>>16;d[k+3|0]=c>>>24;d[k+4|0]=b;d[k+5|0]=b>>>8;d[k+6|0]=b>>>16;d[k+7|0]=b>>>24;b=f[l+12>>2];c=f[l+8>>2];d[k+8|0]=c;d[k+9|0]=c>>>8;d[k+10|0]=c>>>16;d[k+11|0]=c>>>24;d[k+12|0]=b;d[k+13|0]=b>>>8;d[k+14|0]=b>>>16;d[k+15|0]=b>>>24;b=f[l+60>>2];c=f[l+56>>2];d[k+56|0]=c;d[k+57|0]=c>>>8;d[k+58|0]=c>>>16;d[k+59|0]=c>>>24;d[k+60|0]=b;d[k+61|0]=b>>>8;d[k+62|0]=b>>>16;d[k+63|0]=b>>>24;b=f[l+52>>2];c=f[l+48>>2];d[k+48|0]=c;d[k+49|0]=c>>>8;d[k+50|0]=c>>>16;d[k+51|0]=c>>>24;d[k+52|0]=b;d[k+53|0]=b>>>8;d[k+54|0]=b>>>16;d[k+55|0]=b>>>24;b=f[l+44>>2];c=f[l+40>>2];d[k+40|0]=c;d[k+41|0]=c>>>8;d[k+42|0]=c>>>16;d[k+43|0]=c>>>24;d[k+44|0]=b;d[k+45|0]=b>>>8;d[k+46|0]=b>>>16;d[k+47|0]=b>>>24;b=f[l+36>>2];c=f[l+32>>2];d[k+32|0]=c;d[k+33|0]=c>>>8;d[k+34|0]=c>>>16;d[k+35|0]=c>>>24;d[k+36|0]=b;d[k+37|0]=b>>>8;d[k+38|0]=b>>>16;d[k+39|0]=b>>>24;b=f[l+28>>2];c=f[l+24>>2];d[k+24|0]=c;d[k+25|0]=c>>>8;d[k+26|0]=c>>>16;d[k+27|0]=c>>>24;d[k+28|0]=b;d[k+29|0]=b>>>8;d[k+30|0]=b>>>16;d[k+31|0]=b>>>24;b=f[l+20>>2];c=f[l+16>>2];d[k+16|0]=c;d[k+17|0]=c>>>8;d[k+18|0]=c>>>16;d[k+19|0]=c>>>24;d[k+20|0]=b;d[k+21|0]=b>>>8;d[k+22|0]=b>>>16;d[k+23|0]=b>>>24}b=g[a+92|0]|g[a+93|0]<<8|(g[a+94|0]<<16|g[a+95|0]<<24);c=g[a+88|0]|g[a+89|0]<<8|(g[a+90|0]<<16|g[a+91|0]<<24);d[k+88|0]=c;d[k+89|0]=c>>>8;d[k+90|0]=c>>>16;d[k+91|0]=c>>>24;d[k+92|0]=b;d[k+93|0]=b>>>8;d[k+94|0]=b>>>16;d[k+95|0]=b>>>24;b=g[a+84|0]|g[a+85|0]<<8|(g[a+86|0]<<16|g[a+87|0]<<24);c=g[a+80|0]|g[a+81|0]<<8|(g[a+82|0]<<16|g[a+83|0]<<24);d[k+80|0]=c;d[k+81|0]=c>>>8;d[k+82|0]=c>>>16;d[k+83|0]=c>>>24;d[k+84|0]=b;d[k+85|0]=b>>>8;d[k+86|0]=b>>>16;d[k+87|0]=b>>>24;b=g[a+76|0]|g[a+77|0]<<8|(g[a+78|0]<<16|g[a+79|0]<<24);c=g[a+72|0]|g[a+73|0]<<8|(g[a+74|0]<<16|g[a+75|0]<<24);d[k+72|0]=c;d[k+73|0]=c>>>8;d[k+74|0]=c>>>16;d[k+75|0]=c>>>24;d[k+76|0]=b;d[k+77|0]=b>>>8;d[k+78|0]=b>>>16;d[k+79|0]=b>>>24;b=g[a+68|0]|g[a+69|0]<<8|(g[a+70|0]<<16|g[a+71|0]<<24);c=g[a+64|0]|g[a+65|0]<<8|(g[a+66|0]<<16|g[a+67|0]<<24);d[k+64|0]=c;d[k+65|0]=c>>>8;d[k+66|0]=c>>>16;d[k+67|0]=c>>>24;d[k+68|0]=b;d[k+69|0]=b>>>8;d[k+70|0]=b>>>16;d[k+71|0]=b>>>24;b=g[a+100|0]|g[a+101|0]<<8|(g[a+102|0]<<16|g[a+103|0]<<24);c=g[a+96|0]|g[a+97|0]<<8|(g[a+98|0]<<16|g[a+99|0]<<24);d[k+96|0]=c;d[k+97|0]=c>>>8;d[k+98|0]=c>>>16;d[k+99|0]=c>>>24;d[k+100|0]=b;d[k+101|0]=b>>>8;d[k+102|0]=b>>>16;d[k+103|0]=b>>>24;b=g[a+108|0]|g[a+109|0]<<8|(g[a+110|0]<<16|g[a+111|0]<<24);c=g[a+104|0]|g[a+105|0]<<8|(g[a+106|0]<<16|g[a+107|0]<<24);d[k+104|0]=c;d[k+105|0]=c>>>8;d[k+106|0]=c>>>16;d[k+107|0]=c>>>24;d[k+108|0]=b;d[k+109|0]=b>>>8;d[k+110|0]=b>>>16;d[k+111|0]=b>>>24;b=g[a+116|0]|g[a+117|0]<<8|(g[a+118|0]<<16|g[a+119|0]<<24);c=g[a+112|0]|g[a+113|0]<<8|(g[a+114|0]<<16|g[a+115|0]<<24);d[k+112|0]=c;d[k+113|0]=c>>>8;d[k+114|0]=c>>>16;d[k+115|0]=c>>>24;d[k+116|0]=b;d[k+117|0]=b>>>8;d[k+118|0]=b>>>16;d[k+119|0]=b>>>24;b=g[a+124|0]|g[a+125|0]<<8|(g[a+126|0]<<16|g[a+127|0]<<24);a=g[a+120|0]|g[a+121|0]<<8|(g[a+122|0]<<16|g[a+123|0]<<24);d[k+120|0]=a;d[k+121|0]=a>>>8;d[k+122|0]=a>>>16;d[k+123|0]=a>>>24;d[k+124|0]=b;d[k+125|0]=b>>>8;d[k+126|0]=b>>>16;d[k+127|0]=b>>>24;A=l+288|0}function Na(a,b,c){var e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0;e=A-880|0;A=e;a:{if(!ia(e+160|0,a)){break a}if(!ia(e,b)){break a}a=f[e+200>>2];b=f[e+160>>2];k=(a-b|0)+134217690|0;f[e+320>>2]=k&67108863;h=k>>>26|0;k=f[e+204>>2];p=f[e+164>>2];i=((h+k|0)-p|0)+67108862|0;f[e+324>>2]=i&33554431;q=f[e+208>>2];r=f[e+168>>2];i=(q-r+(i>>>25)|0)+134217726|0;f[e+328>>2]=i&67108863;s=f[e+212>>2];t=f[e+172>>2];i=(s-t+(i>>>26)|0)+67108862|0;f[e+332>>2]=i&33554431;u=f[e+216>>2];j=f[e+176>>2];f[e+336>>2]=(u-j+(i>>>25)|0)+134217726;i=f[e+220>>2];v=f[e+180>>2];f[e+340>>2]=(i-v|0)+67108862;w=f[e+224>>2];h=f[e+184>>2];f[e+344>>2]=(w-h|0)+134217726;x=f[e+228>>2];y=f[e+188>>2];f[e+348>>2]=(x-y|0)+67108862;z=f[e+232>>2];m=f[e+192>>2];f[e+352>>2]=(z-m|0)+134217726;n=f[e+236>>2];o=f[e+196>>2];f[e+356>>2]=(n-o|0)+67108862;f[e+868>>2]=n+o;f[e+864>>2]=z+m;f[e+860>>2]=x+y;f[e+856>>2]=h+w;f[e+852>>2]=i+v;f[e+848>>2]=j+u;f[e+844>>2]=s+t;f[e+840>>2]=q+r;f[e+836>>2]=k+p;f[e+832>>2]=a+b;a=f[e+40>>2];b=f[e>>2];k=(a-b|0)+134217690|0;f[e+688>>2]=k&67108863;h=k>>>26|0;k=f[e+44>>2];p=f[e+4>>2];i=((h+k|0)-p|0)+67108862|0;f[e+692>>2]=i&33554431;q=f[e+48>>2];r=f[e+8>>2];i=(q-r+(i>>>25)|0)+134217726|0;f[e+696>>2]=i&67108863;s=f[e+52>>2];t=f[e+12>>2];i=(s-t+(i>>>26)|0)+67108862|0;f[e+700>>2]=i&33554431;u=f[e+56>>2];j=f[e+16>>2];f[e+704>>2]=(u-j+(i>>>25)|0)+134217726;i=f[e+60>>2];v=f[e+20>>2];f[e+708>>2]=(i-v|0)+67108862;w=f[e- -64>>2];h=f[e+24>>2];f[e+712>>2]=(w-h|0)+134217726;x=f[e+68>>2];y=f[e+28>>2];f[e+716>>2]=(x-y|0)+67108862;z=f[e+72>>2];m=f[e+32>>2];f[e+720>>2]=(z-m|0)+134217726;n=f[e+76>>2];o=f[e+36>>2];f[e+724>>2]=(n-o|0)+67108862;f[e+676>>2]=n+o;f[e+672>>2]=z+m;f[e+668>>2]=x+y;f[e+664>>2]=h+w;f[e+660>>2]=i+v;f[e+656>>2]=j+u;f[e+652>>2]=s+t;f[e+648>>2]=q+r;f[e+644>>2]=k+p;f[e+640>>2]=a+b;M(e+320|0,e+320|0,e+688|0);M(e+832|0,e+832|0,e+640|0);M(e+784|0,e+280|0,e+120|0);M(e+784|0,e+784|0,29616);M(e+736|0,e+240|0,e+80|0);a=f[e+772>>2]<<1;f[e+772>>2]=a;b=f[e+768>>2]<<1;f[e+768>>2]=b;k=f[e+764>>2]<<1;f[e+764>>2]=k;p=f[e+760>>2]<<1;f[e+760>>2]=p;i=f[e+756>>2]<<1;f[e+756>>2]=i;q=f[e+752>>2]<<1;f[e+752>>2]=q;r=f[e+748>>2]<<1;f[e+748>>2]=r;s=f[e+744>>2]<<1;f[e+744>>2]=s;t=f[e+736>>2]<<1;f[e+736>>2]=t;u=f[e+740>>2]<<1;f[e+740>>2]=u;w=f[e+832>>2];h=f[e+320>>2];j=(w-h|0)+134217690|0;f[e+480>>2]=j&67108863;x=f[e+836>>2];y=f[e+324>>2];j=((x+(j>>>26|0)|0)-y|0)+67108862|0;f[e+484>>2]=j&33554431;z=f[e+840>>2];m=f[e+328>>2];j=(z-m+(j>>>25)|0)+134217726|0;f[e+488>>2]=j&67108863;n=f[e+844>>2];o=f[e+332>>2];j=(n-o+(j>>>26)|0)+67108862|0;f[e+492>>2]=j&33554431;B=f[e+848>>2];C=f[e+336>>2];f[e+496>>2]=(B-C+(j>>>25)|0)+134217726;D=f[e+852>>2];E=f[e+340>>2];f[e+500>>2]=(D-E|0)+67108862;F=f[e+856>>2];G=f[e+344>>2];f[e+504>>2]=(F-G|0)+134217726;H=f[e+860>>2];I=f[e+348>>2];f[e+508>>2]=(H-I|0)+67108862;J=f[e+864>>2];K=f[e+352>>2];f[e+512>>2]=(J-K|0)+134217726;j=f[e+356>>2];v=f[e+868>>2];f[e+552>>2]=J+K;f[e+548>>2]=H+I;f[e+544>>2]=F+G;f[e+540>>2]=D+E;f[e+536>>2]=B+C;f[e+532>>2]=n+o;f[e+528>>2]=z+m;f[e+524>>2]=x+y;f[e+556>>2]=j+v;f[e+520>>2]=h+w;f[e+516>>2]=(v-j|0)+67108862;j=f[e+788>>2];v=f[e+784>>2];w=v+t|0;h=j+u+(w>>>26)|0;f[e+564>>2]=h&33554431;x=f[e+792>>2];h=x+s+(h>>>25)|0;f[e+568>>2]=h&67108863;y=f[e+796>>2];h=y+r+(h>>>26)|0;f[e+572>>2]=h&33554431;z=f[e+800>>2];h=z+q+(h>>>25)|0;f[e+576>>2]=h&67108863;m=f[e+804>>2];h=m+i+(h>>>26)|0;f[e+580>>2]=h&33554431;n=f[e+808>>2];h=n+p+(h>>>25)|0;f[e+584>>2]=h&67108863;o=f[e+812>>2];h=o+k+(h>>>26)|0;f[e+588>>2]=h&33554431;B=f[e+816>>2];h=B+b+(h>>>25)|0;f[e+592>>2]=h&67108863;C=f[e+820>>2];h=C+a+(h>>>26)|0;f[e+596>>2]=h&33554431;C=a-C|0;B=b-B|0;o=k-o|0;n=p-n|0;m=i-m|0;a=(t-v|0)+268435380|0;b=(u-j+(a>>>26)|0)+134217724|0;k=(s-x+(b>>>25)|0)+268435452|0;p=(r-y+(k>>>26)|0)+134217724|0;i=(q-z+(p>>>25)|0)+268435452|0;q=(m+(i>>>26|0)|0)+134217724|0;r=(n+(q>>>25|0)|0)+268435452|0;s=(o+(r>>>26|0)|0)+134217724|0;t=(B+(s>>>25|0)|0)+268435452|0;u=(C+(t>>>26|0)|0)+134217724|0;f[e+636>>2]=u&33554431;f[e+632>>2]=t&67108863;f[e+628>>2]=s&33554431;f[e+624>>2]=r&67108863;f[e+620>>2]=q&33554431;f[e+616>>2]=i&67108863;f[e+612>>2]=p&33554431;f[e+608>>2]=k&67108863;f[e+604>>2]=b&33554431;f[e+560>>2]=l(h>>>25|0,19)+(w&67108863);f[e+600>>2]=l(u>>>25|0,19)+(a&67108863);a=e+600|0;M(e+320|0,e+480|0,a);b=e+520|0;k=e+560|0;M(e+360|0,b,k);M(e+400|0,k,a);M(e+440|0,e+480|0,b);da(c,e+320|0);d[c+31|0]=g[c+31|0]^128}A=e+880|0}function ia(a,b){var c=0,e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0;c=A-336|0;A=c;t=g[b+31|0];i=f[b+4>>2];h=f[b+8>>2];j=f[b+20>>2];e=f[b+24>>2];k=f[b+28>>2];m=f[b>>2];n=f[b+12>>2];o=f[b+16>>2];f[a+84>>2]=0;f[a+88>>2]=0;f[a+80>>2]=1;f[a+92>>2]=0;f[a+96>>2]=0;f[a+100>>2]=0;f[a+104>>2]=0;f[a+108>>2]=0;f[a+112>>2]=0;f[a+116>>2]=0;f[a+60>>2]=o&33554431;f[a+56>>2]=n>>>6;f[a+40>>2]=m&67108863;f[a+76>>2]=k>>>6&33554431;f[a+68>>2]=((e&524287)<<13|j>>>19)&33554431;b=j;j=o;f[a- -64>>2]=((b&33554431)<<7|j>>>25)&67108863;f[a+48>>2]=((h&524287)<<13|i>>>19)&67108863;b=i;i=m;f[a+44>>2]=((b&67108863)<<6|i>>>26)&33554431;f[a+72>>2]=((k&4095)<<20|e>>>12)&67108863;f[a+52>>2]=((n&8191)<<19|h>>>13)&33554431;b=a+40|0;U(c+96|0,b);M(c+48|0,c+96|0,25648);i=f[a+80>>2];h=(f[c+96>>2]-i|0)+268435380|0;j=f[a+84>>2];e=((f[c+100>>2]+(h>>>26|0)|0)-j|0)+134217724|0;f[c+100>>2]=e&33554431;k=f[a+88>>2];e=(f[c+104>>2]-k+(e>>>25)|0)+268435452|0;f[c+104>>2]=e&67108863;m=f[a+92>>2];e=(f[c+108>>2]-m+(e>>>26)|0)+134217724|0;f[c+108>>2]=e&33554431;n=f[a+96>>2];e=(f[c+112>>2]-n+(e>>>25)|0)+268435452|0;f[c+112>>2]=e&67108863;o=f[a+100>>2];e=(f[c+116>>2]-o+(e>>>26)|0)+134217724|0;f[c+116>>2]=e&33554431;p=f[a+104>>2];e=(f[c+120>>2]-p+(e>>>25)|0)+268435452|0;f[c+120>>2]=e&67108863;q=f[a+108>>2];e=(f[c+124>>2]-q+(e>>>26)|0)+134217724|0;f[c+124>>2]=e&33554431;r=f[a+112>>2];e=(f[c+128>>2]-r+(e>>>25)|0)+268435452|0;f[c+128>>2]=e&67108863;s=f[a+116>>2];e=(f[c+132>>2]-s+(e>>>26)|0)+134217724|0;f[c+132>>2]=e&33554431;h=l(e>>>25|0,19)+(h&67108863)|0;f[c+96>>2]=h;f[c+48>>2]=i+f[c+48>>2];f[c+52>>2]=j+f[c+52>>2];f[c+56>>2]=k+f[c+56>>2];f[c+60>>2]=m+f[c+60>>2];f[c+64>>2]=n+f[c+64>>2];f[c+68>>2]=o+f[c+68>>2];f[c+72>>2]=p+f[c+72>>2];f[c+76>>2]=q+f[c+76>>2];f[c+80>>2]=r+f[c+80>>2];f[c+84>>2]=s+f[c+84>>2];U(c+144|0,c+48|0);M(c,c+144|0,c+48|0);U(a,c);M(a,a,c+48|0);M(a,a,c+96|0);S(c+240|0,a,1);S(c+192|0,c+240|0,2);M(c+288|0,c+192|0,a);M(c+240|0,c+288|0,c+240|0);S(c+192|0,c+240|0,1);M(c+288|0,c+192|0,c+288|0);qa(c+288|0);S(c+288|0,c+288|0,2);M(a,c+288|0,a);M(a,a,c);M(a,a,c+96|0);U(c+144|0,a);M(c+144|0,c+144|0,c+48|0);i=(f[c+144>>2]-h|0)+268435380|0;h=f[c+148>>2];j=f[c+100>>2];e=(((i>>>26|0)+h|0)-j|0)+134217724|0;f[c+292>>2]=e&33554431;k=f[c+152>>2];m=f[c+104>>2];e=(k-m+(e>>>25)|0)+268435452|0;f[c+296>>2]=e&67108863;n=f[c+156>>2];o=f[c+108>>2];e=(n-o+(e>>>26)|0)+134217724|0;f[c+300>>2]=e&33554431;p=f[c+160>>2];q=f[c+112>>2];e=(p-q+(e>>>25)|0)+268435452|0;f[c+304>>2]=e&67108863;r=f[c+164>>2];s=f[c+116>>2];e=(r-s+(e>>>26)|0)+134217724|0;f[c+308>>2]=e&33554431;u=f[c+168>>2];v=f[c+120>>2];e=(u-v+(e>>>25)|0)+268435452|0;f[c+312>>2]=e&67108863;w=f[c+172>>2];x=f[c+124>>2];e=(w-x+(e>>>26)|0)+134217724|0;f[c+316>>2]=e&33554431;y=f[c+176>>2];z=f[c+128>>2];e=(y-z+(e>>>25)|0)+268435452|0;f[c+320>>2]=e&67108863;B=f[c+180>>2];C=f[c+132>>2];e=(B-C+(e>>>26)|0)+134217724|0;f[c+324>>2]=e&33554431;f[c+288>>2]=l(e>>>25|0,19)+(i&67108863);aa(c+240|0,c+288|0);a:{if(!ha(c+240|0,25696)){i=f[c+96>>2]+f[c+144>>2]|0;h=j+((i>>>26|0)+h|0)|0;f[c+148>>2]=h&33554431;h=m+(k+(h>>>25|0)|0)|0;f[c+152>>2]=h&67108863;h=n+o+(h>>>26)|0;f[c+156>>2]=h&33554431;h=p+q+(h>>>25)|0;f[c+160>>2]=h&67108863;h=r+s+(h>>>26)|0;f[c+164>>2]=h&33554431;h=u+v+(h>>>25)|0;f[c+168>>2]=h&67108863;h=w+x+(h>>>26)|0;f[c+172>>2]=h&33554431;h=y+z+(h>>>25)|0;f[c+176>>2]=h&67108863;h=B+C+(h>>>26)|0;f[c+180>>2]=h&33554431;f[c+144>>2]=l(h>>>25|0,19)+(i&67108863);aa(c+240|0,c+144|0);if(!ha(c+240|0,25696)){break a}M(a,a,25728)}aa(c+240|0,a);if((d[c+240|0]&1)==(t>>>7|0)){i=f[a>>2];f[c+144>>2]=i;h=f[a+4>>2];f[c+148>>2]=h;j=f[a+8>>2];f[c+152>>2]=j;e=f[a+12>>2];f[c+156>>2]=e;k=f[a+16>>2];f[c+160>>2]=k;m=f[a+20>>2];f[c+164>>2]=m;n=f[a+24>>2];f[c+168>>2]=n;o=f[a+28>>2];f[c+172>>2]=o;p=f[a+32>>2];f[c+176>>2]=p;t=f[a+36>>2];f[c+180>>2]=t;i=134217690-i|0;h=((i>>>26|0)-h|0)+67108862|0;j=((h>>>25|0)-j|0)+134217726|0;e=((j>>>26|0)-e|0)+67108862|0;k=((e>>>25|0)-k|0)+134217726|0;m=((k>>>26|0)-m|0)+67108862|0;f[a+20>>2]=m&33554431;f[a+16>>2]=k&67108863;f[a+12>>2]=e&33554431;f[a+8>>2]=j&67108863;f[a+4>>2]=h&33554431;h=((m>>>25|0)-n|0)+134217726|0;f[a+24>>2]=h&67108863;h=((h>>>26|0)-o|0)+67108862|0;f[a+28>>2]=h&33554431;h=((h>>>25|0)-p|0)+134217726|0;f[a+32>>2]=h&67108863;h=((h>>>26|0)-t|0)+67108862|0;f[a+36>>2]=h&33554431;f[a>>2]=l(h>>>25|0,19)+(i&67108863)}M(a+120|0,a,b);D=1}A=c+336|0;return D}function Ja(a,b){var c=0,d=0,e=0,g=0,h=0,i=0;e=32;d=A-208|0;A=d;c=f[a+4>>2];h=f[a>>2];a:{b:{if((c|0)==(h|0)){T(a+8|0,0,0);break b}i=c-h|0;if(!h|i>>>0>32){break a}c=a;e=f[c+12>>2];f[d>>2]=f[c+8>>2];f[d+4>>2]=e;e=f[c+20>>2];f[d+8>>2]=f[c+16>>2];f[d+12>>2]=e;e=f[c+28>>2];f[d+16>>2]=f[c+24>>2];f[d+20>>2]=e;e=f[c+36>>2];f[d+24>>2]=f[c+32>>2];f[d+28>>2]=e;e=f[c+44>>2];f[d+32>>2]=f[c+40>>2];f[d+36>>2]=e;e=f[c+52>>2];f[d+40>>2]=f[c+48>>2];f[d+44>>2]=e;e=f[c+60>>2];f[d+48>>2]=f[c+56>>2];f[d+52>>2]=e;e=c- -64|0;g=f[e+4>>2];f[d+56>>2]=f[e>>2];f[d+60>>2]=g;e=f[c+76>>2];f[d+64>>2]=f[c+72>>2];f[d+68>>2]=e;e=f[c+84>>2];f[d+72>>2]=f[c+80>>2];f[d+76>>2]=e;e=f[c+92>>2];f[d+80>>2]=f[c+88>>2];f[d+84>>2]=e;e=f[c+100>>2];f[d+88>>2]=f[c+96>>2];f[d+92>>2]=e;e=f[c+108>>2];f[d+96>>2]=f[c+104>>2];f[d+100>>2]=e;e=f[c+116>>2];f[d+104>>2]=f[c+112>>2];f[d+108>>2]=e;e=f[c+124>>2];f[d+112>>2]=f[c+120>>2];f[d+116>>2]=e;e=f[c+132>>2];f[d+120>>2]=f[c+128>>2];f[d+124>>2]=e;e=f[c+140>>2];f[d+128>>2]=f[c+136>>2];f[d+132>>2]=e;e=f[c+148>>2];f[d+136>>2]=f[c+144>>2];f[d+140>>2]=e;e=f[c+156>>2];f[d+144>>2]=f[c+152>>2];f[d+148>>2]=e;e=f[c+164>>2];f[d+152>>2]=f[c+160>>2];f[d+156>>2]=e;e=f[c+172>>2];f[d+160>>2]=f[c+168>>2];f[d+164>>2]=e;e=f[c+180>>2];f[d+168>>2]=f[c+176>>2];f[d+172>>2]=e;e=f[c+188>>2];f[d+176>>2]=f[c+184>>2];f[d+180>>2]=e;e=f[c+196>>2];f[d+184>>2]=f[c+192>>2];f[d+188>>2]=e;e=f[c+204>>2];f[d+192>>2]=f[c+200>>2];f[d+196>>2]=e;b=P(b,d+h|0,i);T(c+8|0,0,0);b=b+i|0;e=32-i|0}f[a>>2]=0}h=f[a+4>>2];if(e>>>0>h>>>0){i=a+8|0;while(1){c=a;g=f[c+12>>2];f[d>>2]=f[c+8>>2];f[d+4>>2]=g;g=f[c+20>>2];f[d+8>>2]=f[c+16>>2];f[d+12>>2]=g;g=f[c+28>>2];f[d+16>>2]=f[c+24>>2];f[d+20>>2]=g;g=f[c+36>>2];f[d+24>>2]=f[c+32>>2];f[d+28>>2]=g;g=f[c+44>>2];f[d+32>>2]=f[c+40>>2];f[d+36>>2]=g;g=f[c+52>>2];f[d+40>>2]=f[c+48>>2];f[d+44>>2]=g;g=f[c+60>>2];f[d+48>>2]=f[c+56>>2];f[d+52>>2]=g;g=f[c+68>>2];f[d+56>>2]=f[c+64>>2];f[d+60>>2]=g;g=f[c+76>>2];f[d+64>>2]=f[c+72>>2];f[d+68>>2]=g;g=f[c+84>>2];f[d+72>>2]=f[c+80>>2];f[d+76>>2]=g;g=f[c+92>>2];f[d+80>>2]=f[c+88>>2];f[d+84>>2]=g;g=f[c+100>>2];f[d+88>>2]=f[c+96>>2];f[d+92>>2]=g;g=f[c+108>>2];f[d+96>>2]=f[c+104>>2];f[d+100>>2]=g;g=f[c+116>>2];f[d+104>>2]=f[c+112>>2];f[d+108>>2]=g;g=f[c+124>>2];f[d+112>>2]=f[c+120>>2];f[d+116>>2]=g;g=f[c+132>>2];f[d+120>>2]=f[c+128>>2];f[d+124>>2]=g;g=f[c+140>>2];f[d+128>>2]=f[c+136>>2];f[d+132>>2]=g;g=f[c+148>>2];f[d+136>>2]=f[c+144>>2];f[d+140>>2]=g;g=f[c+156>>2];f[d+144>>2]=f[c+152>>2];f[d+148>>2]=g;g=f[c+164>>2];f[d+152>>2]=f[c+160>>2];f[d+156>>2]=g;g=f[c+172>>2];f[d+160>>2]=f[c+168>>2];f[d+164>>2]=g;g=f[c+180>>2];f[d+168>>2]=f[c+176>>2];f[d+172>>2]=g;g=f[c+188>>2];f[d+176>>2]=f[c+184>>2];f[d+180>>2]=g;g=f[c+196>>2];f[d+184>>2]=f[c+192>>2];f[d+188>>2]=g;g=f[c+204>>2];f[d+192>>2]=f[c+200>>2];f[d+196>>2]=g;b=P(b,d,h);T(i,0,0);h=f[c+4>>2];b=b+h|0;e=e-h|0;if(e>>>0>h>>>0){continue}break}}if(e){c=f[a+12>>2];f[d>>2]=f[a+8>>2];f[d+4>>2]=c;c=f[a+20>>2];f[d+8>>2]=f[a+16>>2];f[d+12>>2]=c;c=f[a+28>>2];f[d+16>>2]=f[a+24>>2];f[d+20>>2]=c;c=f[a+36>>2];f[d+24>>2]=f[a+32>>2];f[d+28>>2]=c;c=f[a+44>>2];f[d+32>>2]=f[a+40>>2];f[d+36>>2]=c;c=f[a+52>>2];f[d+40>>2]=f[a+48>>2];f[d+44>>2]=c;c=f[a+60>>2];f[d+48>>2]=f[a+56>>2];f[d+52>>2]=c;c=a- -64|0;h=f[c+4>>2];f[d+56>>2]=f[c>>2];f[d+60>>2]=h;c=f[a+76>>2];f[d+64>>2]=f[a+72>>2];f[d+68>>2]=c;c=f[a+84>>2];f[d+72>>2]=f[a+80>>2];f[d+76>>2]=c;c=f[a+92>>2];f[d+80>>2]=f[a+88>>2];f[d+84>>2]=c;c=f[a+100>>2];f[d+88>>2]=f[a+96>>2];f[d+92>>2]=c;c=f[a+108>>2];f[d+96>>2]=f[a+104>>2];f[d+100>>2]=c;c=f[a+116>>2];f[d+104>>2]=f[a+112>>2];f[d+108>>2]=c;c=f[a+124>>2];f[d+112>>2]=f[a+120>>2];f[d+116>>2]=c;c=f[a+132>>2];f[d+120>>2]=f[a+128>>2];f[d+124>>2]=c;c=f[a+140>>2];f[d+128>>2]=f[a+136>>2];f[d+132>>2]=c;c=f[a+148>>2];f[d+136>>2]=f[a+144>>2];f[d+140>>2]=c;c=f[a+156>>2];f[d+144>>2]=f[a+152>>2];f[d+148>>2]=c;c=f[a+164>>2];f[d+152>>2]=f[a+160>>2];f[d+156>>2]=c;c=f[a+172>>2];f[d+160>>2]=f[a+168>>2];f[d+164>>2]=c;c=f[a+180>>2];f[d+168>>2]=f[a+176>>2];f[d+172>>2]=c;c=f[a+188>>2];f[d+176>>2]=f[a+184>>2];f[d+180>>2]=c;c=f[a+196>>2];f[d+184>>2]=f[a+192>>2];f[d+188>>2]=c;c=f[a+204>>2];f[d+192>>2]=f[a+200>>2];f[d+196>>2]=c;P(b,d+f[a>>2]|0,e);f[a>>2]=f[a>>2]+e}A=d+208|0}function S(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0;i=f[b+36>>2];k=f[b+32>>2];m=f[b+28>>2];j=f[b+24>>2];g=f[b+20>>2];h=f[b+16>>2];w=f[b+12>>2];q=f[b+8>>2];y=f[b+4>>2];b=f[b>>2];while(1){z=g<<1;D=l(j,19);d=hb(z,0,D);p=B;n=h;C=l(m,38);o=hb(h,0,C);e=o+d|0;d=B+p|0;d=e>>>0<o>>>0?d+1|0:d;x=w<<1;A=l(k,19);o=hb(x,0,A);p=o+e|0;e=B+d|0;e=p>>>0<o>>>0?e+1|0:e;d=p;s=l(i,38);p=hb(s,0,q&2147483647);d=d+p|0;e=B+e|0;e=d>>>0<p>>>0?e+1|0:e;t=b<<1;G=y;o=hb(t,0,y);p=o+d|0;d=B+e|0;r=p;o=p>>>0<o>>>0?d+1|0:d;E=h<<1;d=hb(E,0,D);e=B;p=g;h=hb(g,0,l(g,38));d=h+d|0;g=B+e|0;g=d>>>0<h>>>0?g+1|0:g;h=hb(x,0,C);e=h+d|0;d=B+g|0;d=e>>>0<h>>>0?d+1|0:d;u=q<<1;h=hb(u,0,A);g=h+e|0;e=B+d|0;e=g>>>0<h>>>0?e+1|0:e;d=g;v=y<<1;g=hb(v,0,s);d=d+g|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;g=hb(b,0,b);b=g+d|0;d=B+e|0;d=b>>>0<g>>>0?d+1|0:d;y=b;e=d>>>26|0;d=(d&67108863)<<6|b>>>26;b=d+r|0;g=e+o|0;F=b;b=b>>>0<d>>>0?g+1|0:g;h=j;d=hb(x,0,h);g=B;o=hb(n,0,z);e=o+d|0;d=B+g|0;d=e>>>0<o>>>0?d+1|0:d;o=m;r=hb(u,0,m);g=r+e|0;e=B+d|0;e=g>>>0<r>>>0?e+1|0:e;r=hb(v,0,k);g=r+g|0;d=B+e|0;d=g>>>0<r>>>0?d+1|0:d;r=hb(t,0,i);g=r+g|0;e=B+d|0;H=g;r=g>>>0<r>>>0?e+1|0:e;d=hb(n,0,n);e=B;i=hb(s,0,i);d=i+d|0;g=B+e|0;g=d>>>0<i>>>0?g+1|0:g;i=hb(x,0,z);e=i+d|0;d=B+g|0;d=e>>>0<i>>>0?d+1|0:d;i=hb(u,0,h);g=i+e|0;e=B+d|0;e=g>>>0<i>>>0?e+1|0:e;m=m<<1;i=hb(v,0,m);g=i+g|0;d=B+e|0;d=g>>>0<i>>>0?d+1|0:d;i=hb(t,0,k);g=i+g|0;e=B+d|0;I=g;J=g>>>0<i>>>0?e+1|0:e;d=hb(n,0,x);e=B;i=hb(s,0,k);d=i+d|0;g=B+e|0;g=d>>>0<i>>>0?g+1|0:g;i=hb(u,0,p);e=i+d|0;d=B+g|0;d=e>>>0<i>>>0?d+1|0:d;i=hb(v,0,h);g=i+e|0;e=B+d|0;e=g>>>0<i>>>0?e+1|0:e;i=hb(t,0,o);g=i+g|0;d=B+e|0;K=g;i=g>>>0<i>>>0?d+1|0:d;d=hb(s,0,m);e=B;g=hb(A,0,k);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;k=hb(x,0,w);d=k+d|0;g=B+e|0;g=d>>>0<k>>>0?g+1|0:g;k=hb(u,0,n);e=k+d|0;d=B+g|0;d=e>>>0<k>>>0?d+1|0:d;k=hb(v,0,z);g=k+e|0;e=B+d|0;e=g>>>0<k>>>0?e+1|0:e;k=hb(t,0,h);g=k+g|0;d=B+e|0;L=g;k=g>>>0<k>>>0?d+1|0:d;d=hb(s,0,h);e=B;g=hb(A,0,m);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;m=hb(u,0,w);d=m+d|0;g=B+e|0;g=d>>>0<m>>>0?g+1|0:g;m=hb(n,0,v);e=m+d|0;d=B+g|0;d=e>>>0<m>>>0?d+1|0:d;m=hb(t,0,p);g=m+e|0;e=B+d|0;p=g;m=g>>>0<m>>>0?e+1|0:e;d=hb(h<<1,0,A);g=B;j=hb(C,0,o);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;j=hb(s,0,z);g=j+e|0;e=B+d|0;e=g>>>0<j>>>0?e+1|0:e;j=hb(q,0,q);d=j+g|0;g=B+e|0;g=d>>>0<j>>>0?g+1|0:g;j=hb(v,0,x);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;j=hb(n,0,t);g=j+e|0;e=B+d|0;o=g;j=g>>>0<j>>>0?e+1|0:e;d=hb(z,0,A);g=B;u=hb(h,0,C);e=u+d|0;d=B+g|0;n=hb(n,0,s);g=n+e|0;e=B+(e>>>0<u>>>0?d+1|0:d)|0;e=g>>>0<n>>>0?e+1|0:e;n=hb(q,0,v);d=n+g|0;g=B+e|0;g=d>>>0<n>>>0?g+1|0:g;n=hb(t,0,w);e=n+d|0;d=B+g|0;w=e;n=e>>>0<n>>>0?d+1|0:d;d=hb(z,0,C);e=B;g=hb(h,0,D);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;h=hb(A,0,E);g=h+d|0;d=B+e|0;d=g>>>0<h>>>0?d+1|0:d;h=hb(s,0,x);g=h+g|0;e=B+d|0;e=g>>>0<h>>>0?e+1|0:e;h=hb(v,0,G);d=h+g|0;g=B+e|0;g=d>>>0<h>>>0?g+1|0:g;h=hb(q,0,t);e=h+d|0;d=B+g|0;d=e>>>0<h>>>0?d+1|0:d;h=e;e=b>>>25|0;g=(b&33554431)<<7|F>>>25;b=h+g|0;e=d+e|0;e=b>>>0<g>>>0?e+1|0:e;g=b;d=e>>>26|0;g=(e&67108863)<<6|g>>>26;e=g+w|0;d=d+n|0;d=e>>>0<g>>>0?d+1|0:d;n=e;g=e;e=d>>>25|0;h=(d&33554431)<<7|g>>>25;d=h+o|0;g=e+j|0;g=d>>>0<h>>>0?g+1|0:g;h=d;e=d;d=g>>>26|0;j=(g&67108863)<<6|e>>>26;g=j+p|0;e=d+m|0;m=g;d=g;e=d>>>0<j>>>0?e+1|0:e;g=e>>>25|0;j=(e&33554431)<<7|d>>>25;e=j+L|0;d=g+k|0;d=e>>>0<j>>>0?d+1|0:d;j=e;g=e;e=d>>>26|0;g=(d&67108863)<<6|g>>>26;d=g+K|0;e=e+i|0;e=d>>>0<g>>>0?e+1|0:e;i=d;g=d;d=e>>>25|0;g=(e&33554431)<<7|g>>>25;e=g+I|0;d=d+J|0;d=e>>>0<g>>>0?d+1|0:d;k=e;g=e;e=d>>>26|0;q=(d&67108863)<<6|g>>>26;d=q+H|0;g=e+r|0;g=d>>>0<q>>>0?g+1|0:g;q=y&67108863;g=hb((g&33554431)<<7|d>>>25,0,19)+q|0;e=B;e=g>>>0<q>>>0?e+1|0:e;p=g;y=((e&67108863)<<6|g>>>26)+(F&33554431)|0;q=b&67108863;w=n&33554431;h=h&67108863;g=m&33554431;j=j&67108863;m=i&33554431;k=k&67108863;i=d&33554431;b=p&67108863;c=c+ -1|0;if(c){continue}break}f[a+36>>2]=i;f[a+32>>2]=k;f[a+28>>2]=m;f[a+24>>2]=j;f[a+20>>2]=g;f[a+16>>2]=h;f[a+12>>2]=w;f[a+8>>2]=q;f[a+4>>2]=y;f[a>>2]=b}function U(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0;e=a;p=f[b+8>>2];D=p;k=f[b>>2];r=k<<1;a=hb(p,0,r);q=B;c=a;a=f[b+4>>2];s=a<<1;j=a;h=hb(s,0,a);c=c+h|0;a=B+q|0;a=c>>>0<h>>>0?a+1|0:a;h=f[b+24>>2];z=l(h,19);q=h;m=hb(z,0,h);c=m+c|0;d=B+a|0;d=c>>>0<m>>>0?d+1|0:d;a=c;m=f[b+28>>2];A=l(m,38);c=f[b+20>>2];w=c<<1;u=hb(A,0,w);g=a+u|0;a=B+d|0;a=g>>>0<u>>>0?a+1|0:a;o=g;u=f[b+32>>2];x=l(u,19);g=f[b+16>>2];C=g<<1;i=hb(x,0,C);y=o+i|0;a=B+a|0;a=y>>>0<i>>>0?a+1|0:a;o=y;y=f[b+36>>2];t=l(y,38);i=f[b+12>>2];v=i<<1;n=hb(t,0,v);o=o+n|0;b=B+a|0;E=o;o=o>>>0<n>>>0?b+1|0:b;b=hb(z,0,w);a=B;j=hb(r,0,j);b=j+b|0;a=B+a|0;a=b>>>0<j>>>0?a+1|0:a;j=g;g=hb(A,0,g);b=g+b|0;d=B+a|0;d=b>>>0<g>>>0?d+1|0:d;g=hb(x,0,v);b=g+b|0;a=B+d|0;a=b>>>0<g>>>0?a+1|0:a;g=hb(t,0,p&2147483647);b=g+b|0;a=B+a|0;a=b>>>0<g>>>0?a+1|0:a;n=a;g=c;a=hb(c,0,l(c,38));c=B;F=b;k=hb(k,0,k);a=k+a|0;b=B+c|0;b=a>>>0<k>>>0?b+1|0:b;k=hb(z,0,C);c=k+a|0;a=B+b|0;a=c>>>0<k>>>0?a+1|0:a;b=c;c=hb(A,0,v);b=b+c|0;d=B+a|0;d=b>>>0<c>>>0?d+1|0:d;p=p<<1;c=hb(x,0,p);b=c+b|0;a=B+d|0;a=b>>>0<c>>>0?a+1|0:a;c=hb(t,0,s);b=c+b|0;a=B+a|0;a=b>>>0<c>>>0?a+1|0:a;z=b;d=a>>>26|0;c=(a&67108863)<<6|b>>>26;a=F+c|0;b=d+n|0;b=a>>>0<c>>>0?b+1|0:b;C=a;c=a;a=b>>>25|0;c=(b&33554431)<<7|c>>>25;b=c+E|0;a=a+o|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[e+8>>2]=b&67108863;k=e;e=i;b=hb(e,0,r);a=B;i=hb(s,0,D);b=i+b|0;a=B+a|0;a=b>>>0<i>>>0?a+1|0:a;i=hb(A,0,h);b=i+b|0;d=B+a|0;d=b>>>0<i>>>0?d+1|0:d;i=hb(x,0,w);a=i+b|0;b=B+d|0;b=a>>>0<i>>>0?b+1|0:b;n=hb(j,0,t);i=n+a|0;a=B+b|0;d=c>>>26|0;c=(c&67108863)<<6|o>>>26;b=c+i|0;a=d+(i>>>0<n>>>0?a+1|0:a)|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[k+12>>2]=b&33554431;i=k;b=hb(s,0,v);a=B;k=hb(D,0,D);b=k+b|0;a=B+a|0;a=b>>>0<k>>>0?a+1|0:a;n=hb(r,0,j);k=n+b|0;b=B+a|0;b=k>>>0<n>>>0?b+1|0:b;a=k;k=m;d=hb(A,0,k);n=a+d|0;a=B+b|0;h=hb(x,0,h<<1);b=h+n|0;d=B+(n>>>0<d>>>0?a+1|0:a)|0;d=b>>>0<h>>>0?d+1|0:d;h=hb(t,0,w);b=h+b|0;a=B+d|0;a=b>>>0<h>>>0?a+1|0:a;h=b;b=a;m=h;a=c>>>25|0;h=(c&33554431)<<7|o>>>25;c=m+h|0;a=b+a|0;o=c;c=c>>>0<h>>>0?a+1|0:a;f[i+16>>2]=o&67108863;h=i;b=hb(j,0,s);i=B;n=hb(p,0,e);a=n+b|0;b=B+i|0;b=a>>>0<n>>>0?b+1|0:b;i=hb(r,0,g);a=i+a|0;d=B+b|0;d=a>>>0<i>>>0?d+1|0:d;i=k<<1;m=hb(x,0,i);b=m+a|0;a=B+d|0;a=b>>>0<m>>>0?a+1|0:a;m=hb(t,0,q);b=m+b|0;a=B+a|0;a=b>>>0<m>>>0?a+1|0:a;d=c>>>26|0;c=(c&67108863)<<6|o>>>26;b=c+b|0;a=a+d|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[h+20>>2]=b&33554431;m=h;b=hb(v,0,e);h=B;e=hb(j,0,p);a=e+b|0;b=B+h|0;b=a>>>0<e>>>0?b+1|0:b;e=hb(s,0,w);h=e+a|0;a=B+b|0;a=h>>>0<e>>>0?a+1|0:a;b=h;h=hb(r,0,q);b=b+h|0;a=B+a|0;a=b>>>0<h>>>0?a+1|0:a;h=u;e=hb(x,0,h);b=e+b|0;d=B+a|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(t,0,i);b=e+b|0;a=B+d|0;a=b>>>0<e>>>0?a+1|0:a;e=b;b=a;n=e;a=c>>>25|0;e=(c&33554431)<<7|o>>>25;c=n+e|0;b=b+a|0;b=c>>>0<e>>>0?b+1|0:b;f[m+24>>2]=c&67108863;a=hb(p,0,g);e=B;u=hb(j,0,v);g=u+a|0;a=B+e|0;a=g>>>0<u>>>0?a+1|0:a;e=hb(s,0,q);g=e+g|0;d=B+a|0;d=g>>>0<e>>>0?d+1|0:d;e=hb(r,0,k);g=e+g|0;a=B+d|0;a=g>>>0<e>>>0?a+1|0:a;e=hb(h,0,t);g=e+g|0;a=B+a|0;a=g>>>0<e>>>0?a+1|0:a;e=g;g=(b&67108863)<<6|c>>>26;c=e+g|0;b=(b>>>26|0)+a|0;b=c>>>0<g>>>0?b+1|0:b;g=c;c=b;f[m+28>>2]=g&33554431;b=hb(p,0,q);a=B;e=hb(j,0,j);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=hb(v,0,w);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=hb(s,0,i);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=hb(r,0,h);b=e+b|0;d=B+a|0;d=b>>>0<e>>>0?d+1|0:d;e=hb(t,0,y);a=e+b|0;b=B+d|0;b=a>>>0<e>>>0?b+1|0:b;e=a;a=c>>>25|0;g=(c&33554431)<<7|g>>>25;c=e+g|0;a=b+a|0;a=c>>>0<g>>>0?a+1|0:a;g=c;c=a;f[m+32>>2]=g&67108863;b=hb(j,0,w);a=B;j=hb(v,0,q);b=j+b|0;a=B+a|0;a=b>>>0<j>>>0?a+1|0:a;j=hb(p,0,k);b=j+b|0;d=B+a|0;d=b>>>0<j>>>0?d+1|0:d;j=hb(h,0,s);a=j+b|0;b=B+d|0;b=a>>>0<j>>>0?b+1|0:b;q=hb(r,0,y);j=q+a|0;a=B+b|0;d=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=c+j|0;a=d+(j>>>0<q>>>0?a+1|0:a)|0;a=b>>>0<c>>>0?a+1|0:a;f[m+36>>2]=b&33554431;c=z&67108863;b=hb((a&33554431)<<7|b>>>25,0,19)+c|0;a=B;a=b>>>0<c>>>0?a+1|0:a;f[m>>2]=b&67108863;f[m+4>>2]=(C&33554431)+((a&67108863)<<6|b>>>26)}function sa(a,b){var c=0,e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0;c=A-352|0;A=c;h=f[b>>2];i=h&15;d[c+128|0]=i;d[c+134|0]=h>>>24&15;d[c+133|0]=h>>>20&15;d[c+132|0]=h>>>16&15;d[c+131|0]=h>>>12&15;d[c+130|0]=h>>>8&15;d[c+129|0]=(h&240)>>>4;e=f[b+4>>2];d[c+142|0]=e>>>26&15;d[c+141|0]=e>>>22&15;d[c+140|0]=e>>>18&15;d[c+139|0]=e>>>14&15;d[c+138|0]=e>>>10&15;d[c+137|0]=e>>>6&15;d[c+136|0]=e>>>2&15;d[c+135|0]=(e<<2|h>>>28)&15;h=f[b+8>>2];d[c+143|0]=h&15;d[c+149|0]=h>>>24&15;d[c+148|0]=h>>>20&15;d[c+147|0]=h>>>16&15;d[c+146|0]=h>>>12&15;d[c+145|0]=h>>>8&15;d[c+144|0]=(h&240)>>>4;e=f[b+12>>2];d[c+157|0]=e>>>26&15;d[c+156|0]=e>>>22&15;d[c+155|0]=e>>>18&15;d[c+154|0]=e>>>14&15;d[c+153|0]=e>>>10&15;d[c+152|0]=e>>>6&15;d[c+151|0]=e>>>2&15;d[c+150|0]=(e<<2|h>>>28)&15;h=f[b+16>>2];d[c+158|0]=h&15;d[c+164|0]=h>>>24&15;d[c+163|0]=h>>>20&15;d[c+162|0]=h>>>16&15;d[c+161|0]=h>>>12&15;d[c+160|0]=h>>>8&15;d[c+159|0]=(h&240)>>>4;e=f[b+20>>2];d[c+172|0]=e>>>26&15;d[c+171|0]=e>>>22&15;d[c+170|0]=e>>>18&15;d[c+169|0]=e>>>14&15;d[c+168|0]=e>>>10&15;d[c+167|0]=e>>>6&15;d[c+166|0]=e>>>2&15;d[c+165|0]=(e<<2|h>>>28)&15;h=f[b+24>>2];d[c+173|0]=h&15;d[c+179|0]=h>>>24&15;d[c+178|0]=h>>>20&15;d[c+177|0]=h>>>16&15;d[c+176|0]=h>>>12&15;d[c+175|0]=h>>>8&15;d[c+174|0]=(h&240)>>>4;e=f[b+28>>2];d[c+187|0]=e>>>26&15;d[c+186|0]=e>>>22&15;d[c+185|0]=e>>>18&15;d[c+184|0]=e>>>14&15;d[c+183|0]=e>>>10&15;d[c+182|0]=e>>>6&15;d[c+181|0]=e>>>2&15;d[c+180|0]=(e<<2|h>>>28)&15;b=f[b+32>>2];d[c+188|0]=b&15;d[c+191|0]=b>>>12&15;d[c+190|0]=b>>>8&15;d[c+189|0]=(b&240)>>>4;b=0;e=0;while(1){h=b+1|0;j=h+(c+128|0)|0;e=e+i|0;i=g[j|0]+(e<<24>>28)|0;d[j|0]=i;j=e&15;e=(e&8)>>>3|0;d[(c+128|0)+b|0]=j-(e<<4);b=h;if((b|0)!=63){continue}break}d[c+191|0]=g[c+191|0]+e;ca(c+8|0,0,d[c+129|0]);e=f[c+48>>2];i=f[c+8>>2];b=(e-i|0)+268435380|0;j=b&67108863;f[a>>2]=j;k=f[c+52>>2];m=f[c+12>>2];b=((k+(b>>>26|0)|0)-m|0)+134217724|0;f[a+4>>2]=b&33554431;n=f[c+56>>2];o=f[c+16>>2];b=(n-o+(b>>>25)|0)+268435452|0;f[a+8>>2]=b&67108863;p=f[c+60>>2];q=f[c+20>>2];b=(p-q+(b>>>26)|0)+134217724|0;f[a+12>>2]=b&33554431;r=f[c- -64>>2];s=f[c+24>>2];b=(r-s+(b>>>25)|0)+268435452|0;f[a+16>>2]=b&67108863;t=f[c+68>>2];u=f[c+28>>2];b=(t-u+(b>>>26)|0)+134217724|0;f[a+20>>2]=b&33554431;v=f[c+72>>2];w=f[c+32>>2];b=(v-w+(b>>>25)|0)+268435452|0;f[a+24>>2]=b&67108863;x=f[c+76>>2];y=f[c+36>>2];b=(x-y+(b>>>26)|0)+134217724|0;f[a+28>>2]=b&33554431;z=f[c+80>>2];B=f[c+40>>2];C=(z-B+(b>>>25)|0)+268435452|0;f[a+32>>2]=C&67108863;b=f[c+84>>2];h=f[c+44>>2];e=e+i|0;i=m+(k+(e>>>26|0)|0)|0;k=o+(n+(i>>>25|0)|0)|0;m=p+q+(k>>>26)|0;n=r+s+(m>>>25)|0;o=t+u+(n>>>26)|0;p=v+w+(o>>>25)|0;q=x+y+(p>>>26)|0;r=z+B+(q>>>25)|0;f[a+72>>2]=r&67108863;f[a+68>>2]=q&33554431;f[a- -64>>2]=p&67108863;f[a+60>>2]=o&33554431;f[a+56>>2]=n&67108863;f[a+52>>2]=m&33554431;f[a+48>>2]=k&67108863;f[a+44>>2]=i&33554431;f[a+84>>2]=0;f[a+88>>2]=0;f[a+92>>2]=0;f[a+96>>2]=0;f[a+100>>2]=0;f[a+104>>2]=0;f[a+108>>2]=0;f[a+112>>2]=0;f[a+116>>2]=0;i=b+h+(r>>>26)|0;f[a+76>>2]=i&33554431;b=(b-h+(C>>>26)|0)+134217724|0;f[a+36>>2]=b&33554431;f[a+40>>2]=l(i>>>25|0,19)+(e&67108863);f[a>>2]=j+l(b>>>25|0,19);f[a+120>>2]=f[c+88>>2];f[a+124>>2]=f[c+92>>2];f[a+128>>2]=f[c+96>>2];f[a+132>>2]=f[c+100>>2];f[a+136>>2]=f[c+104>>2];f[a+140>>2]=f[c+108>>2];f[a+144>>2]=f[c+112>>2];f[a+148>>2]=f[c+116>>2];f[a+152>>2]=f[c+120>>2];b=f[c+124>>2];f[a+80>>2]=2;f[a+156>>2]=b;m=a+120|0;e=a+80|0;i=a+40|0;k=c+88|0;b=3;while(1){ca(c+8|0,b>>>1|0,d[(c+128|0)+b|0]);ja(a,c+8|0);h=b>>>0<62;b=b+2|0;if(h){continue}break}_(c+192|0,a);b=c+312|0;M(a,c+192|0,b);j=c+232|0;h=c+272|0;M(i,j,h);M(e,h,b);_(c+192|0,a);M(a,c+192|0,b);M(i,j,h);M(e,h,b);_(c+192|0,a);M(a,c+192|0,b);M(i,j,h);M(e,h,b);_(c+192|0,a);M(a,c+192|0,b);M(i,j,h);M(e,h,b);M(m,c+192|0,j);ca(c+8|0,0,d[c+128|0]);M(k,k,25648);ja(a,c+8|0);b=2;while(1){ca(c+8|0,b>>>1|0,d[(c+128|0)+b|0]);ja(a,c+8|0);h=b>>>0<62;b=b+2|0;if(h){continue}break}A=c+352|0}function Fa(a,b,c,e,h,i){var j=0,k=0,l=0;j=A-880|0;A=j;if((c|0)<0){a=1}else{a:{b:{switch(i+ -1|0){case 0:c=c<<8&16711680|c<<24|(c>>>8&65280|c>>>24);d[j+76|0]=c;d[j+77|0]=c>>>8;d[j+78|0]=c>>>16;d[j+79|0]=c>>>24;break a;case 1:break b;default:break a}}d[j+76|0]=c;d[j+77|0]=c>>>8;d[j+78|0]=c>>>16;d[j+79|0]=c>>>24}c=b;k=g[c+20|0]|g[c+21|0]<<8|(g[c+22|0]<<16|g[c+23|0]<<24);f[j+768>>2]=g[c+16|0]|g[c+17|0]<<8|(g[c+18|0]<<16|g[c+19|0]<<24);f[j+772>>2]=k;k=g[c+28|0]|g[c+29|0]<<8|(g[c+30|0]<<16|g[c+31|0]<<24);f[j+776>>2]=g[c+24|0]|g[c+25|0]<<8|(g[c+26|0]<<16|g[c+27|0]<<24);f[j+780>>2]=k;k=g[c+4|0]|g[c+5|0]<<8|(g[c+6|0]<<16|g[c+7|0]<<24);f[j+752>>2]=g[c|0]|g[c+1|0]<<8|(g[c+2|0]<<16|g[c+3|0]<<24);f[j+756>>2]=k;k=g[c+12|0]|g[c+13|0]<<8|(g[c+14|0]<<16|g[c+15|0]<<24);f[j+760>>2]=g[c+8|0]|g[c+9|0]<<8|(g[c+10|0]<<16|g[c+11|0]<<24);f[j+764>>2]=k;c=0;l=O(j+784|0,0,96);while(1){k=g[(j+752|0)+c|0];d[(j+624|0)+c|0]=k^54;d[(j+496|0)+c|0]=k^92;c=c+1|0;if((c|0)!=128){continue}break}R(j+80|0);N(j+80|0,j+624|0,128);k=j+288|0;R(k);N(k,j+496|0,128);N(j+80|0,1026,1);N(j+80|0,a,32);N(j+80|0,j+76|0,4);Q(j+80|0,j);N(k,j,64);Q(k,j);f[j+768>>2]=0;f[j+772>>2]=0;f[j+776>>2]=0;f[j+780>>2]=0;f[j+752>>2]=0;f[j+756>>2]=0;f[j+760>>2]=0;f[j+764>>2]=0;c:{d:{switch(i+ -1|0){case 0:d[j+752|0]=g[j|0]<<3;d[j+753|0]=g[j+1|0]<<3;d[j+754|0]=g[j+2|0]<<3;d[j+755|0]=g[j+3|0]<<3;d[j+756|0]=g[j+4|0]<<3;d[j+757|0]=g[j+5|0]<<3;d[j+758|0]=g[j+6|0]<<3;d[j+759|0]=g[j+7|0]<<3;d[j+760|0]=g[j+8|0]<<3;d[j+761|0]=g[j+9|0]<<3;d[j+762|0]=g[j+10|0]<<3;d[j+763|0]=g[j+11|0]<<3;d[j+764|0]=g[j+12|0]<<3;d[j+765|0]=g[j+13|0]<<3;d[j+766|0]=g[j+14|0]<<3;d[j+767|0]=g[j+15|0]<<3;d[j+768|0]=g[j+16|0]<<3;d[j+769|0]=g[j+17|0]<<3;d[j+770|0]=g[j+18|0]<<3;d[j+771|0]=g[j+19|0]<<3;d[j+772|0]=g[j+20|0]<<3;d[j+773|0]=g[j+21|0]<<3;d[j+774|0]=g[j+22|0]<<3;d[j+775|0]=g[j+23|0]<<3;d[j+776|0]=g[j+24|0]<<3;d[j+777|0]=g[j+25|0]<<3;d[j+778|0]=g[j+26|0]<<3;d[j+779|0]=g[j+27|0]<<3;d[j+780|0]=g[j+28|0]<<3;d[j+781|0]=g[j+29|0]<<3;d[j+782|0]=g[j+30|0]<<3;d[j+783|0]=g[j+31|0]<<3;break c;case 1:break d;default:break c}}ma(j+752|0,j)}ba(j+752|0,j+624|0);Na(j+624|0,a,e);c=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[j+768>>2]=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);f[j+772>>2]=c;c=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);f[j+776>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);f[j+780>>2]=c;c=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[j+752>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[j+756>>2]=c;c=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[j+760>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[j+764>>2]=c;c=0;O(l,0,96);while(1){b=g[(j+752|0)+c|0];d[(j+624|0)+c|0]=b^54;d[(j+496|0)+c|0]=b^92;c=c+1|0;if((c|0)!=128){continue}break}R(j+80|0);N(j+80|0,j+624|0,128);R(k);N(k,j+496|0,128);N(j+80|0,1030,1);N(j+80|0,a,32);N(j+80|0,j+76|0,4);Q(j+80|0,j+752|0);N(k,j+752|0,64);Q(k,j+752|0);a=f[j+812>>2];b=f[j+808>>2];d[h+24|0]=b;d[h+25|0]=b>>>8;d[h+26|0]=b>>>16;d[h+27|0]=b>>>24;d[h+28|0]=a;d[h+29|0]=a>>>8;d[h+30|0]=a>>>16;d[h+31|0]=a>>>24;a=f[j+804>>2];b=f[j+800>>2];d[h+16|0]=b;d[h+17|0]=b>>>8;d[h+18|0]=b>>>16;d[h+19|0]=b>>>24;d[h+20|0]=a;d[h+21|0]=a>>>8;d[h+22|0]=a>>>16;d[h+23|0]=a>>>24;a=f[j+796>>2];b=f[j+792>>2];d[h+8|0]=b;d[h+9|0]=b>>>8;d[h+10|0]=b>>>16;d[h+11|0]=b>>>24;d[h+12|0]=a;d[h+13|0]=a>>>8;d[h+14|0]=a>>>16;d[h+15|0]=a>>>24;a=f[j+788>>2];b=f[j+784>>2];d[h|0]=b;d[h+1|0]=b>>>8;d[h+2|0]=b>>>16;d[h+3|0]=b>>>24;d[h+4|0]=a;d[h+5|0]=a>>>8;d[h+6|0]=a>>>16;d[h+7|0]=a>>>24;a=0}A=j+880|0;return a}function la(a,b,c,e){var h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0;H=A+ -64|0;A=H;if(e){I=f[a+60>>2];J=f[a+56>>2];z=f[a+52>>2];B=f[a+48>>2];K=f[a+44>>2];L=f[a+40>>2];M=f[a+36>>2];N=f[a+32>>2];O=f[a+28>>2];Q=f[a+24>>2];R=f[a+20>>2];S=f[a+16>>2];T=f[a+12>>2];U=f[a+8>>2];V=f[a+4>>2];W=f[a>>2];while(1){a:{if(e>>>0>63){h=c;break a}h=P(H,b,e);b=h;X=c}C=20;j=W;m=V;n=U;q=T;c=S;l=R;r=Q;s=O;k=N;w=M;o=L;i=I;x=J;t=z;p=B;u=K;while(1){v=c;c=c+j|0;j=ib(c^p,16);k=j+k|0;p=ib(v^k,12);v=p;y=c+p|0;p=ib(y^j,8);k=p+k|0;c=ib(v^k,7);j=s+q|0;i=ib(j^i,16);u=i+u|0;s=ib(u^s,12);n=r+n|0;q=ib(n^x,16);o=q+o|0;r=ib(o^r,12);D=j+s|0;x=D+c|0;n=r+n|0;E=ib(n^q,8);j=ib(x^E,16);m=l+m|0;q=ib(m^t,16);w=q+w|0;l=ib(w^l,12);v=c;m=l+m|0;t=ib(m^q,8);F=t+w|0;c=F+j|0;G=ib(v^c,12);q=G+x|0;x=ib(j^q,8);w=x+c|0;c=ib(w^G,7);v=k;k=n;j=ib(i^D,8);i=j+u|0;n=ib(i^s,7);k=k+n|0;t=ib(k^t,16);s=v+t|0;u=ib(s^n,12);n=u+k|0;t=ib(t^n,8);k=s+t|0;s=ib(k^u,7);v=i;i=m;o=o+E|0;m=ib(o^r,7);i=i+m|0;p=ib(i^p,16);r=v+p|0;v=i;i=ib(r^m,12);m=v+i|0;p=ib(p^m,8);u=r+p|0;r=ib(u^i,7);v=o;i=j;j=ib(l^F,7);o=j+y|0;i=ib(i^o,16);l=v+i|0;y=ib(j^l,12);j=y+o|0;i=ib(i^j,8);o=l+i|0;l=ib(o^y,7);C=C+ -2|0;if(C){continue}break}C=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);y=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);D=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);E=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);F=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);G=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);Y=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);Z=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);_=g[b+32|0]|g[b+33|0]<<8|(g[b+34|0]<<16|g[b+35|0]<<24);$=g[b+36|0]|g[b+37|0]<<8|(g[b+38|0]<<16|g[b+39|0]<<24);aa=g[b+40|0]|g[b+41|0]<<8|(g[b+42|0]<<16|g[b+43|0]<<24);ba=g[b+44|0]|g[b+45|0]<<8|(g[b+46|0]<<16|g[b+47|0]<<24);ca=g[b+48|0]|g[b+49|0]<<8|(g[b+50|0]<<16|g[b+51|0]<<24);da=g[b+52|0]|g[b+53|0]<<8|(g[b+54|0]<<16|g[b+55|0]<<24);v=g[b+56|0]|g[b+57|0]<<8|(g[b+58|0]<<16|g[b+59|0]<<24);i=i+I^(g[b+60|0]|g[b+61|0]<<8|(g[b+62|0]<<16|g[b+63|0]<<24));d[h+60|0]=i;x=x+J^v;d[h+56|0]=x;t=t+z^da;d[h+52|0]=t;p=p+B^ca;d[h+48|0]=p;u=u+K^ba;d[h+44|0]=u;o=o+L^aa;d[h+40|0]=o;w=w+M^$;d[h+36|0]=w;k=k+N^_;d[h+32|0]=k;s=s+O^Z;d[h+28|0]=s;r=r+Q^Y;d[h+24|0]=r;l=G^l+R;d[h+20|0]=l;c=F^c+S;d[h+16|0]=c;q=E^q+T;d[h+12|0]=q;n=D^n+U;d[h+8|0]=n;m=y^m+V;d[h+4|0]=m;j=C^j+W;d[h|0]=j;d[h+63|0]=i>>>24;d[h+62|0]=i>>>16;d[h+61|0]=i>>>8;d[h+59|0]=x>>>24;d[h+58|0]=x>>>16;d[h+57|0]=x>>>8;d[h+55|0]=t>>>24;d[h+54|0]=t>>>16;d[h+53|0]=t>>>8;d[h+51|0]=p>>>24;d[h+50|0]=p>>>16;d[h+49|0]=p>>>8;d[h+47|0]=u>>>24;d[h+46|0]=u>>>16;d[h+45|0]=u>>>8;d[h+43|0]=o>>>24;d[h+42|0]=o>>>16;d[h+41|0]=o>>>8;d[h+39|0]=w>>>24;d[h+38|0]=w>>>16;d[h+37|0]=w>>>8;d[h+35|0]=k>>>24;d[h+34|0]=k>>>16;d[h+33|0]=k>>>8;d[h+31|0]=s>>>24;d[h+30|0]=s>>>16;d[h+29|0]=s>>>8;d[h+27|0]=r>>>24;d[h+26|0]=r>>>16;d[h+25|0]=r>>>8;d[h+23|0]=l>>>24;d[h+22|0]=l>>>16;d[h+21|0]=l>>>8;d[h+19|0]=c>>>24;d[h+18|0]=c>>>16;d[h+17|0]=c>>>8;d[h+15|0]=q>>>24;d[h+14|0]=q>>>16;d[h+13|0]=q>>>8;d[h+11|0]=n>>>24;d[h+10|0]=n>>>16;d[h+9|0]=n>>>8;d[h+7|0]=m>>>24;d[h+6|0]=m>>>16;d[h+5|0]=m>>>8;d[h+3|0]=j>>>24;d[h+2|0]=j>>>16;d[h+1|0]=j>>>8;l=B+1|0;z=(l>>>0<B>>>0)+z|0;if(e>>>0<=64){if(e>>>0<=63){P(X,h,e)}f[a+52>>2]=z;f[a+48>>2]=l}else{b=b- -64|0;c=h- -64|0;e=e+ -64|0;B=l;continue}break}}A=H- -64|0}function ja(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0;c=A-336|0;A=c;h=f[a+40>>2];i=f[a>>2];d=(h-i|0)+134217690|0;f[c+288>>2]=d&67108863;j=f[a+44>>2];k=f[a+4>>2];d=((j+(d>>>26|0)|0)-k|0)+67108862|0;f[c+292>>2]=d&33554431;m=f[a+48>>2];n=f[a+8>>2];d=(m-n+(d>>>25)|0)+134217726|0;f[c+296>>2]=d&67108863;o=f[a+52>>2];r=f[a+12>>2];d=(o-r+(d>>>26)|0)+67108862|0;f[c+300>>2]=d&33554431;p=f[a+56>>2];q=f[a+16>>2];f[c+304>>2]=(p-q+(d>>>25)|0)+134217726;s=f[a+60>>2];e=f[a+20>>2];f[c+308>>2]=(s-e|0)+67108862;t=f[a- -64>>2];u=f[a+24>>2];f[c+312>>2]=(t-u|0)+134217726;v=f[a+68>>2];w=f[a+28>>2];f[c+316>>2]=(v-w|0)+67108862;x=f[a+72>>2];y=f[a+32>>2];f[c+320>>2]=(x-y|0)+134217726;d=f[a+76>>2];g=f[a+36>>2];f[c+272>>2]=x+y;f[c+268>>2]=v+w;f[c+264>>2]=t+u;f[c+260>>2]=e+s;f[c+256>>2]=p+q;f[c+252>>2]=o+r;f[c+248>>2]=m+n;f[c+244>>2]=j+k;f[c+240>>2]=h+i;f[c+276>>2]=d+g;f[c+324>>2]=(d-g|0)+67108862;M(c+288|0,c+288|0,b);M(c+144|0,c+240|0,b+40|0);d=f[c+288>>2];g=f[c+144>>2];f[c>>2]=d+g;h=f[c+292>>2];i=f[c+148>>2];f[c+4>>2]=h+i;j=f[c+296>>2];k=f[c+152>>2];f[c+8>>2]=j+k;m=f[c+300>>2];n=f[c+156>>2];f[c+12>>2]=m+n;o=f[c+304>>2];r=f[c+160>>2];f[c+16>>2]=o+r;p=f[c+308>>2];q=f[c+164>>2];f[c+20>>2]=p+q;f[c+164>>2]=(q-p|0)+67108862;d=(g-d|0)+134217690|0;f[c+144>>2]=d&67108863;g=f[c+312>>2];p=f[c+168>>2];f[c+24>>2]=g+p;q=f[c+316>>2];s=f[c+172>>2];f[c+28>>2]=q+s;e=f[c+320>>2];t=f[c+176>>2];f[c+32>>2]=e+t;u=f[c+324>>2];v=f[c+180>>2];f[c+36>>2]=u+v;f[c+172>>2]=(s-q|0)+67108862;f[c+176>>2]=(t-e|0)+134217726;f[c+180>>2]=(v-u|0)+67108862;f[c+168>>2]=(p-g|0)+134217726;d=((i+(d>>>26|0)|0)-h|0)+67108862|0;f[c+148>>2]=d&33554431;d=(k-j+(d>>>25)|0)+134217726|0;f[c+152>>2]=d&67108863;d=(n-m+(d>>>26)|0)+67108862|0;f[c+156>>2]=d&33554431;f[c+160>>2]=(r-o+(d>>>25)|0)+134217726;r=a+120|0;M(c+192|0,r,b+80|0);b=f[a+80>>2]<<1;f[c+96>>2]=b;d=f[a+84>>2]<<1;f[c+100>>2]=d;g=f[a+88>>2]<<1;f[c+104>>2]=g;h=f[a+92>>2]<<1;f[c+108>>2]=h;i=f[a+96>>2]<<1;f[c+112>>2]=i;j=f[a+100>>2]<<1;f[c+116>>2]=j;k=f[a+104>>2]<<1;f[c+120>>2]=k;m=f[a+108>>2]<<1;f[c+124>>2]=m;n=f[a+112>>2]<<1;f[c+128>>2]=n;o=f[a+116>>2]<<1;f[c+132>>2]=o;s=f[c+196>>2];p=f[c+192>>2];q=p+b|0;e=s+(d+(q>>>26|0)|0)|0;f[c+52>>2]=e&33554431;t=f[c+200>>2];e=t+g+(e>>>25)|0;f[c+56>>2]=e&67108863;u=f[c+204>>2];e=u+h+(e>>>26)|0;f[c+60>>2]=e&33554431;v=f[c+208>>2];e=v+i+(e>>>25)|0;f[c+64>>2]=e&67108863;w=f[c+212>>2];e=w+j+(e>>>26)|0;f[c+68>>2]=e&33554431;x=f[c+216>>2];e=x+k+(e>>>25)|0;f[c+72>>2]=e&67108863;y=f[c+220>>2];e=y+m+(e>>>26)|0;f[c+76>>2]=e&33554431;z=f[c+224>>2];e=n+z+(e>>>25)|0;f[c+80>>2]=e&67108863;B=f[c+228>>2];e=o+B+(e>>>26)|0;f[c+84>>2]=e&33554431;f[c+48>>2]=l(e>>>25|0,19)+(q&67108863);b=(b-p|0)+268435380|0;d=((d+(b>>>26|0)|0)-s|0)+134217724|0;g=(g-t+(d>>>25)|0)+268435452|0;h=(h-u+(g>>>26)|0)+134217724|0;i=(i-v+(h>>>25)|0)+268435452|0;j=(j-w+(i>>>26)|0)+134217724|0;k=(k-x+(j>>>25)|0)+268435452|0;m=(m-y+(k>>>26)|0)+134217724|0;n=(n-z+(m>>>25)|0)+268435452|0;o=(o-B+(n>>>26)|0)+134217724|0;f[c+132>>2]=o&33554431;f[c+128>>2]=n&67108863;f[c+124>>2]=m&33554431;f[c+120>>2]=k&67108863;f[c+116>>2]=j&33554431;f[c+112>>2]=i&67108863;f[c+108>>2]=h&33554431;f[c+104>>2]=g&67108863;f[c+100>>2]=d&33554431;f[c+96>>2]=l(o>>>25|0,19)+(b&67108863);M(a,c+144|0,c+96|0);M(a+40|0,c,c+48|0);M(a+80|0,c+48|0,c+96|0);M(r,c+144|0,c);A=c+336|0}function eb(a,b,c,i,j){var k=0,l=0;k=A-320|0;A=k;l=1;if(!Ma(c,k)){ba(k,k- -64|0);a:{if(b){O(k+96|0,0,131);c=h[529]|h[530]<<16;l=h[527]|h[528]<<16;e[k+302>>1]=l;e[k+304>>1]=l>>>16;e[k+306>>1]=c;e[k+308>>1]=c>>>16;c=f[263];f[k+296>>2]=f[262];f[k+300>>2]=c;l=f[261];c=k;f[c+288>>2]=f[260];f[c+292>>2]=l;X(a,b,c+288|0,c+240|0);Z(c+96|0,c+240|0,c+272|0);f[c+264>>2]=0;f[c+268>>2]=0;f[c+256>>2]=0;f[c+260>>2]=0;f[c+272>>2]=0;f[c+276>>2]=0;f[c+248>>2]=0;f[c+252>>2]=0;f[c+240>>2]=0;f[c+244>>2]=0;Y(j,c+96|0,c,64);O(c+96|0,0,131);break a}a=k;b=f[a+4>>2];c=f[a>>2];d[j|0]=c;d[j+1|0]=c>>>8;d[j+2|0]=c>>>16;d[j+3|0]=c>>>24;d[j+4|0]=b;d[j+5|0]=b>>>8;d[j+6|0]=b>>>16;d[j+7|0]=b>>>24;b=f[a+12>>2];c=f[a+8>>2];d[j+8|0]=c;d[j+9|0]=c>>>8;d[j+10|0]=c>>>16;d[j+11|0]=c>>>24;d[j+12|0]=b;d[j+13|0]=b>>>8;d[j+14|0]=b>>>16;d[j+15|0]=b>>>24;b=f[a+60>>2];c=f[a+56>>2];d[j+56|0]=c;d[j+57|0]=c>>>8;d[j+58|0]=c>>>16;d[j+59|0]=c>>>24;d[j+60|0]=b;d[j+61|0]=b>>>8;d[j+62|0]=b>>>16;d[j+63|0]=b>>>24;b=f[a+52>>2];c=f[a+48>>2];d[j+48|0]=c;d[j+49|0]=c>>>8;d[j+50|0]=c>>>16;d[j+51|0]=c>>>24;d[j+52|0]=b;d[j+53|0]=b>>>8;d[j+54|0]=b>>>16;d[j+55|0]=b>>>24;b=f[a+44>>2];c=f[a+40>>2];d[j+40|0]=c;d[j+41|0]=c>>>8;d[j+42|0]=c>>>16;d[j+43|0]=c>>>24;d[j+44|0]=b;d[j+45|0]=b>>>8;d[j+46|0]=b>>>16;d[j+47|0]=b>>>24;b=f[a+36>>2];c=f[a+32>>2];d[j+32|0]=c;d[j+33|0]=c>>>8;d[j+34|0]=c>>>16;d[j+35|0]=c>>>24;d[j+36|0]=b;d[j+37|0]=b>>>8;d[j+38|0]=b>>>16;d[j+39|0]=b>>>24;b=f[a+28>>2];c=f[a+24>>2];d[j+24|0]=c;d[j+25|0]=c>>>8;d[j+26|0]=c>>>16;d[j+27|0]=c>>>24;d[j+28|0]=b;d[j+29|0]=b>>>8;d[j+30|0]=b>>>16;d[j+31|0]=b>>>24;b=f[a+20>>2];a=f[a+16>>2];d[j+16|0]=a;d[j+17|0]=a>>>8;d[j+18|0]=a>>>16;d[j+19|0]=a>>>24;d[j+20|0]=b;d[j+21|0]=b>>>8;d[j+22|0]=b>>>16;d[j+23|0]=b>>>24}a=f[k+76>>2];b=f[k+72>>2];d[j+72|0]=b;d[j+73|0]=b>>>8;d[j+74|0]=b>>>16;d[j+75|0]=b>>>24;d[j+76|0]=a;d[j+77|0]=a>>>8;d[j+78|0]=a>>>16;d[j+79|0]=a>>>24;a=f[k+68>>2];b=f[k+64>>2];d[j+64|0]=b;d[j+65|0]=b>>>8;d[j+66|0]=b>>>16;d[j+67|0]=b>>>24;d[j+68|0]=a;d[j+69|0]=a>>>8;d[j+70|0]=a>>>16;d[j+71|0]=a>>>24;a=f[k+92>>2];b=f[k+88>>2];d[j+88|0]=b;d[j+89|0]=b>>>8;d[j+90|0]=b>>>16;d[j+91|0]=b>>>24;d[j+92|0]=a;d[j+93|0]=a>>>8;d[j+94|0]=a>>>16;d[j+95|0]=a>>>24;a=f[k+84>>2];b=f[k+80>>2];d[j+80|0]=b;d[j+81|0]=b>>>8;d[j+82|0]=b>>>16;d[j+83|0]=b>>>24;d[j+84|0]=a;d[j+85|0]=a>>>8;d[j+86|0]=a>>>16;d[j+87|0]=a>>>24;a=g[i+4|0]|g[i+5|0]<<8|(g[i+6|0]<<16|g[i+7|0]<<24);b=g[i|0]|g[i+1|0]<<8|(g[i+2|0]<<16|g[i+3|0]<<24);d[j+96|0]=b;d[j+97|0]=b>>>8;d[j+98|0]=b>>>16;d[j+99|0]=b>>>24;d[j+100|0]=a;d[j+101|0]=a>>>8;d[j+102|0]=a>>>16;d[j+103|0]=a>>>24;a=g[i+12|0]|g[i+13|0]<<8|(g[i+14|0]<<16|g[i+15|0]<<24);b=g[i+8|0]|g[i+9|0]<<8|(g[i+10|0]<<16|g[i+11|0]<<24);d[j+104|0]=b;d[j+105|0]=b>>>8;d[j+106|0]=b>>>16;d[j+107|0]=b>>>24;d[j+108|0]=a;d[j+109|0]=a>>>8;d[j+110|0]=a>>>16;d[j+111|0]=a>>>24;a=g[i+20|0]|g[i+21|0]<<8|(g[i+22|0]<<16|g[i+23|0]<<24);b=g[i+16|0]|g[i+17|0]<<8|(g[i+18|0]<<16|g[i+19|0]<<24);d[j+112|0]=b;d[j+113|0]=b>>>8;d[j+114|0]=b>>>16;d[j+115|0]=b>>>24;d[j+116|0]=a;d[j+117|0]=a>>>8;d[j+118|0]=a>>>16;d[j+119|0]=a>>>24;a=g[i+28|0]|g[i+29|0]<<8|(g[i+30|0]<<16|g[i+31|0]<<24);b=g[i+24|0]|g[i+25|0]<<8|(g[i+26|0]<<16|g[i+27|0]<<24);d[j+120|0]=b;d[j+121|0]=b>>>8;d[j+122|0]=b>>>16;d[j+123|0]=b>>>24;d[j+124|0]=a;d[j+125|0]=a>>>8;d[j+126|0]=a>>>16;d[j+127|0]=a>>>24;l=0}A=k+320|0;return l}function _(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0;d=A-144|0;A=d;U(d+96|0,b);U(d+48|0,b+40|0);U(d,b+80|0);g=f[d>>2];c=(g>>>25&63)+(f[d+4>>2]<<1)|0;f[d+4>>2]=c&33554431;c=(f[d+8>>2]<<1)+(c>>>25|0)|0;f[d+8>>2]=c&67108863;c=(f[d+12>>2]<<1)+(c>>>26|0)|0;f[d+12>>2]=c&33554431;c=(f[d+16>>2]<<1)+(c>>>25|0)|0;f[d+16>>2]=c&67108863;c=(f[d+20>>2]<<1)+(c>>>26|0)|0;f[d+20>>2]=c&33554431;c=(f[d+24>>2]<<1)+(c>>>25|0)|0;f[d+24>>2]=c&67108863;c=(f[d+28>>2]<<1)+(c>>>26|0)|0;f[d+28>>2]=c&33554431;c=(f[d+32>>2]<<1)+(c>>>25|0)|0;f[d+32>>2]=c&67108863;c=(f[d+36>>2]<<1)+(c>>>26|0)|0;f[d+36>>2]=c&33554431;f[d>>2]=l(c>>>25|0,19)+(g<<1&67108862);f[a>>2]=f[b+40>>2]+f[b>>2];f[a+4>>2]=f[b+44>>2]+f[b+4>>2];f[a+8>>2]=f[b+48>>2]+f[b+8>>2];f[a+12>>2]=f[b+52>>2]+f[b+12>>2];f[a+16>>2]=f[b+56>>2]+f[b+16>>2];f[a+20>>2]=f[b+60>>2]+f[b+20>>2];f[a+24>>2]=f[b- -64>>2]+f[b+24>>2];f[a+28>>2]=f[b+68>>2]+f[b+28>>2];f[a+32>>2]=f[b+72>>2]+f[b+32>>2];f[a+36>>2]=f[b+76>>2]+f[b+36>>2];U(a,a);c=f[d+96>>2];h=f[d+48>>2];e=c+h|0;f[a+40>>2]=e;i=f[d+100>>2];j=f[d+52>>2];r=i+j|0;f[a+44>>2]=r;k=f[d+104>>2];m=f[d+56>>2];s=k+m|0;f[a+48>>2]=s;t=f[d+108>>2];u=f[d+60>>2];v=t+u|0;f[a+52>>2]=v;w=f[d+112>>2];x=f[d+64>>2];y=w+x|0;f[a+56>>2]=y;n=f[d+68>>2];z=f[d+116>>2];B=n+z|0;f[a+60>>2]=B;o=f[d+72>>2];C=f[d+120>>2];D=o+C|0;f[a- -64>>2]=D;p=f[d+76>>2];E=f[d+124>>2];F=p+E|0;f[a+68>>2]=F;q=f[d+80>>2];G=f[d+128>>2];H=q+G|0;f[a+72>>2]=H;b=f[d+132>>2];g=f[d+84>>2];q=(q-G|0)+134217726|0;f[a+112>>2]=q;p=(p-E|0)+67108862|0;f[a+108>>2]=p;o=(o-C|0)+134217726|0;f[a+104>>2]=o;n=(n-z|0)+67108862|0;f[a+100>>2]=n;c=(h-c|0)+134217690|0;h=((j+(c>>>26|0)|0)-i|0)+67108862|0;i=(m-k+(h>>>25)|0)+134217726|0;j=(u-t+(i>>>26)|0)+67108862|0;k=(x-w+(j>>>25)|0)+134217726|0;f[a+96>>2]=k;j=j&33554431;f[a+92>>2]=j;i=i&67108863;f[a+88>>2]=i;h=h&33554431;f[a+84>>2]=h;c=c&67108863;f[a+80>>2]=c;m=b+g|0;f[a+76>>2]=m;b=(g-b|0)+67108862|0;f[a+116>>2]=b;g=(f[a>>2]-e|0)+268435380|0;e=(f[a+4>>2]-r+(g>>>26)|0)+134217724|0;f[a+4>>2]=e&33554431;e=(f[a+8>>2]-s+(e>>>25)|0)+268435452|0;f[a+8>>2]=e&67108863;e=(f[a+12>>2]-v+(e>>>26)|0)+134217724|0;f[a+12>>2]=e&33554431;e=(f[a+16>>2]-y+(e>>>25)|0)+268435452|0;f[a+16>>2]=e&67108863;e=(f[a+20>>2]-B+(e>>>26)|0)+134217724|0;f[a+20>>2]=e&33554431;e=(f[a+24>>2]-D+(e>>>25)|0)+268435452|0;f[a+24>>2]=e&67108863;e=(f[a+28>>2]-F+(e>>>26)|0)+134217724|0;f[a+28>>2]=e&33554431;e=(f[a+32>>2]-H+(e>>>25)|0)+268435452|0;f[a+32>>2]=e&67108863;e=(f[a+36>>2]-m+(e>>>26)|0)+134217724|0;f[a+36>>2]=e&33554431;f[a>>2]=l(e>>>25|0,19)+(g&67108863);g=(f[d>>2]-c|0)+268435380|0;c=(f[d+4>>2]-h+(g>>>26)|0)+134217724|0;f[a+124>>2]=c&33554431;c=(f[d+8>>2]-i+(c>>>25)|0)+268435452|0;f[a+128>>2]=c&67108863;c=(f[d+12>>2]-j+(c>>>26)|0)+134217724|0;f[a+132>>2]=c&33554431;c=(f[d+16>>2]-k+(c>>>25)|0)+268435452|0;f[a+136>>2]=c&67108863;c=(f[d+20>>2]-n+(c>>>26)|0)+134217724|0;f[a+140>>2]=c&33554431;c=(f[d+24>>2]-o+(c>>>25)|0)+268435452|0;f[a+144>>2]=c&67108863;c=(f[d+28>>2]-p+(c>>>26)|0)+134217724|0;f[a+148>>2]=c&33554431;c=(f[d+32>>2]-q+(c>>>25)|0)+268435452|0;f[a+152>>2]=c&67108863;b=(f[d+36>>2]-b+(c>>>26)|0)+134217724|0;f[a+156>>2]=b&33554431;f[a+120>>2]=l(b>>>25|0,19)+(g&67108863);A=d+144|0}function ca(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0;h=(c&128)>>>7|0;g=0-h|0;B=g^c-h;C=b<<3;n=1;o=1;while(1){b=l(i+C|0,96);i=i+1|0;h=(i^B)+ -1|0;c=h>>31;e=(h>>>31|0)+ -1|0;h=f[b+1104>>2]&c|e&(n|D);n=h&255;F=c&f[b+1072>>2]|e&(o|E);o=F&255;D=h&-256;E=F&-256;d=c&f[b+1164>>2]|d&e;p=c&f[b+1160>>2]|e&p;q=c&f[b+1156>>2]|e&q;j=c&f[b+1152>>2]|e&j;k=c&f[b+1148>>2]|e&k;r=c&f[b+1144>>2]|e&r;y=c&f[b+1140>>2]|e&y;m=c&f[b+1136>>2]|e&m;s=c&f[b+1132>>2]|e&s;z=c&f[b+1128>>2]|e&z;A=c&f[b+1124>>2]|e&A;t=c&f[b+1120>>2]|e&t;u=c&f[b+1116>>2]|e&u;K=c&f[b+1112>>2]|e&K;L=c&f[b+1108>>2]|e&L;v=c&f[b+1100>>2]|e&v;M=c&f[b+1096>>2]|e&M;N=c&f[b+1092>>2]|e&N;w=c&f[b+1088>>2]|e&w;x=c&f[b+1084>>2]|e&x;O=c&f[b+1080>>2]|e&O;P=c&f[b+1076>>2]|e&P;if((i|0)!=8){continue}break}i=j&33554431;f[a+100>>2]=i;e=k>>>6|0;f[a+96>>2]=e;n=m&67108863;f[a+80>>2]=n;B=t&33554431;f[a+60>>2]=B;C=u>>>6|0;f[a+56>>2]=C;D=h&67108863;f[a+40>>2]=D;E=w&33554431;f[a+20>>2]=E;G=x>>>6|0;f[a+16>>2]=G;H=F&67108863;f[a>>2]=H;o=d>>>6&33554431;f[a+116>>2]=o;I=s>>>6&33554431;f[a+76>>2]=I;J=v>>>6&33554431;f[a+36>>2]=J;b=d;c=p;p=((b&4095)<<20|c>>>12)&67108863;f[a+112>>2]=p;b=c;d=q;c=d;q=((b&524287)<<13|c>>>19)&33554431;f[a+108>>2]=q;j=((c&33554431)<<7|j>>>25)&67108863;f[a+104>>2]=j;b=k;c=r;k=((b&8191)<<19|c>>>13)&33554431;f[a+92>>2]=k;b=c;d=y;c=d;r=((b&524287)<<13|c>>>19)&67108863;f[a+88>>2]=r;m=((c&67108863)<<6|m>>>26)&33554431;f[a+84>>2]=m;b=s;c=z;s=((b&4095)<<20|c>>>12)&67108863;f[a+72>>2]=s;b=c;d=A;c=d;y=((b&524287)<<13|c>>>19)&33554431;f[a+68>>2]=y;Q=a- -64|0;t=((c&33554431)<<7|t>>>25)&67108863;f[Q>>2]=t;b=u;c=K;u=((b&8191)<<19|c>>>13)&33554431;f[a+52>>2]=u;b=c;d=L;c=d;z=((b&524287)<<13|c>>>19)&67108863;f[a+48>>2]=z;h=((c&67108863)<<6|h>>>26)&33554431;f[a+44>>2]=h;b=v;c=M;v=((b&4095)<<20|c>>>12)&67108863;f[a+32>>2]=v;b=c;d=N;c=d;A=((b&524287)<<13|c>>>19)&33554431;f[a+28>>2]=A;w=((c&33554431)<<7|w>>>25)&67108863;f[a+24>>2]=w;b=x;c=O;x=((b&8191)<<19|c>>>13)&33554431;f[a+12>>2]=x;b=c;d=P;c=d;c=((b&524287)<<13|c>>>19)&67108863;f[a+8>>2]=c;b=d;d=F;b=((b&67108863)<<6|d>>>26)&33554431;f[a+4>>2]=b;d=(D^H)&g;f[a+40>>2]=d^D;f[a>>2]=d^H;d=(C^G)&g;f[a+56>>2]=d^C;f[a+16>>2]=d^G;d=(B^E)&g;f[a+60>>2]=d^B;f[a+20>>2]=d^E;d=(b^h)&g;f[a+44>>2]=d^h;f[a+4>>2]=b^d;b=(c^z)&g;f[a+48>>2]=b^z;f[a+8>>2]=b^c;b=(u^x)&g;f[a+52>>2]=b^u;f[a+12>>2]=b^x;b=(t^w)&g;f[a+24>>2]=b^w;f[Q>>2]=b^t;b=(y^A)&g;f[a+68>>2]=b^y;f[a+28>>2]=b^A;b=(s^v)&g;f[a+72>>2]=b^s;f[a+32>>2]=b^v;b=(I^J)&g;f[a+76>>2]=b^I;f[a+36>>2]=b^J;b=134217690-n|0;c=((b>>>26|0)-m|0)+67108862|0;f[a+84>>2]=(c&33554431^m)&g^m;c=((c>>>25|0)-r|0)+134217726|0;f[a+88>>2]=r^(r^c&67108863)&g;c=((c>>>26|0)-k|0)+67108862|0;f[a+92>>2]=(c&33554431^k)&g^k;c=((c>>>25|0)-e|0)+134217726|0;f[a+96>>2]=(c&67108863^e)&g^e;c=((c>>>26|0)-i|0)+67108862|0;f[a+100>>2]=(c&33554431^i)&g^i;c=((c>>>25|0)-j|0)+134217726|0;f[a+104>>2]=j^(j^c&67108863)&g;c=((c>>>26|0)-q|0)+67108862|0;f[a+108>>2]=q^(q^c&33554431)&g;c=((c>>>25|0)-p|0)+134217726|0;f[a+112>>2]=p^(p^c&67108863)&g;c=((c>>>26|0)-o|0)+67108862|0;f[a+116>>2]=o^(o^c&33554431)&g;f[a+80>>2]=n^(n^l(c>>>25|0,19)+(b&67108863))&g}function Q(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0;l=A-16|0;A=l;m=f[a+8>>2];g=f[a+12>>2];j=f[a>>2];h=j<<27|j<<11&16711680;c=f[a+4>>2];i=c<<27|j>>>5;e=i&65280;i=c<<11|j>>>21;i=i&255|e|h;h=c<<3|j>>>29;e=l;f[e+8>>2]=((c&31)<<27|j>>>5)&-16777216|((c&2097151)<<11|j>>>21)&16711680|(c>>>5&65280|h>>>24)|k;f[e+12>>2]=i;h=m;i=g<<3|h>>>29;g=h<<3|c>>>29;c=i;m=g<<8&16711680|g<<24;h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|m;f[e>>2]=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|k;f[e+4>>2]=h;c=j&127;N(a,30928,(c>>>0<112?112:240)-c|0);N(a,e,16);j=0;g=f[a+144>>2];k=g<<24|g<<8&16711680;c=f[a+148>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;e=b;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e|0]=c;d[e+1|0]=c>>>8;d[e+2|0]=c>>>16;d[e+3|0]=c>>>24;c=h;d[e+4|0]=c;d[e+5|0]=c>>>8;d[e+6|0]=c>>>16;d[e+7|0]=c>>>24;g=f[a+152>>2];k=g<<24|g<<8&16711680;c=f[a+156>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+8|0]=c;d[e+9|0]=c>>>8;d[e+10|0]=c>>>16;d[e+11|0]=c>>>24;c=h;d[e+12|0]=c;d[e+13|0]=c>>>8;d[e+14|0]=c>>>16;d[e+15|0]=c>>>24;g=f[a+160>>2];k=g<<24|g<<8&16711680;c=f[a+164>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+16|0]=c;d[e+17|0]=c>>>8;d[e+18|0]=c>>>16;d[e+19|0]=c>>>24;c=h;d[e+20|0]=c;d[e+21|0]=c>>>8;d[e+22|0]=c>>>16;d[e+23|0]=c>>>24;g=f[a+168>>2];k=g<<24|g<<8&16711680;c=f[a+172>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+24|0]=c;d[e+25|0]=c>>>8;d[e+26|0]=c>>>16;d[e+27|0]=c>>>24;c=h;d[e+28|0]=c;d[e+29|0]=c>>>8;d[e+30|0]=c>>>16;d[e+31|0]=c>>>24;g=f[a+176>>2];k=g<<24|g<<8&16711680;c=f[a+180>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+32|0]=c;d[e+33|0]=c>>>8;d[e+34|0]=c>>>16;d[e+35|0]=c>>>24;c=h;d[e+36|0]=c;d[e+37|0]=c>>>8;d[e+38|0]=c>>>16;d[e+39|0]=c>>>24;g=f[a+184>>2];k=g<<24|g<<8&16711680;c=f[a+188>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+40|0]=c;d[e+41|0]=c>>>8;d[e+42|0]=c>>>16;d[e+43|0]=c>>>24;c=h;d[e+44|0]=c;d[e+45|0]=c>>>8;d[e+46|0]=c>>>16;d[e+47|0]=c>>>24;g=f[a+192>>2];k=g<<24|g<<8&16711680;c=f[a+196>>2];h=c<<24|g>>>8;i=c<<8|g>>>24;h=h&65280|i&255|k;c=((c&255)<<24|g>>>8)&-16777216|((c&16777215)<<8|g>>>24)&16711680|(c>>>8&65280|c>>>24)|j;d[e+48|0]=c;d[e+49|0]=c>>>8;d[e+50|0]=c>>>16;d[e+51|0]=c>>>24;c=h;d[e+52|0]=c;d[e+53|0]=c>>>8;d[e+54|0]=c>>>16;d[e+55|0]=c>>>24;c=e;b=f[a+204>>2];e=f[a+200>>2];i=e<<24;g=0;j=e<<8&16711680|i;h=b<<24|e>>>8;i=b<<8|e>>>24;a=g;g=h&65280|i&255|j;h=a;a=b;a=h|(((a&255)<<24|e>>>8)&-16777216|((a&16777215)<<8|e>>>24)&16711680|(a>>>8&65280|a>>>24));d[c+56|0]=a;d[c+57|0]=a>>>8;d[c+58|0]=a>>>16;d[c+59|0]=a>>>24;d[c+60|0]=g;d[c+61|0]=g>>>8;d[c+62|0]=g>>>16;d[c+63|0]=g>>>24;A=l+16|0}function wa(a){a=a|0;var b=0,c=0,d=0,e=0,g=0,h=0,j=0;a:{if(!a){break a}d=a+ -8|0;c=f[a+ -4>>2];a=c&-8;g=d+a|0;b:{if(c&1){break b}if(!(c&3)){break a}c=f[d>>2];d=d-c|0;if(d>>>0<i[7769]){break a}a=a+c|0;if(f[7770]!=(d|0)){if(c>>>0<=255){e=f[d+8>>2];c=c>>>3|0;b=f[d+12>>2];if((b|0)==(e|0)){f[7765]=f[7765]&ib(-2,c);break b}f[e+12>>2]=b;f[b+8>>2]=e;break b}j=f[d+24>>2];c=f[d+12>>2];c:{if((d|0)!=(c|0)){b=f[d+8>>2];f[b+12>>2]=c;f[c+8>>2]=b;break c}d:{e=d+20|0;b=f[e>>2];if(b){break d}e=d+16|0;b=f[e>>2];if(b){break d}c=0;break c}while(1){h=e;c=b;e=c+20|0;b=f[e>>2];if(b){continue}e=c+16|0;b=f[c+16>>2];if(b){continue}break}f[h>>2]=0}if(!j){break b}e=f[d+28>>2];b=(e<<2)+31364|0;e:{if(f[b>>2]==(d|0)){f[b>>2]=c;if(c){break e}f[7766]=f[7766]&ib(-2,e);break b}f[j+(f[j+16>>2]==(d|0)?16:20)>>2]=c;if(!c){break b}}f[c+24>>2]=j;b=f[d+16>>2];if(b){f[c+16>>2]=b;f[b+24>>2]=c}b=f[d+20>>2];if(!b){break b}f[c+20>>2]=b;f[b+24>>2]=c;break b}c=f[g+4>>2];if((c&3)!=3){break b}f[7767]=a;f[g+4>>2]=c&-2;f[d+4>>2]=a|1;f[a+d>>2]=a;return}if(g>>>0<=d>>>0){break a}c=f[g+4>>2];if(!(c&1)){break a}f:{if(!(c&2)){if(f[7771]==(g|0)){f[7771]=d;a=f[7768]+a|0;f[7768]=a;f[d+4>>2]=a|1;if(f[7770]!=(d|0)){break a}f[7767]=0;f[7770]=0;return}if(f[7770]==(g|0)){f[7770]=d;a=f[7767]+a|0;f[7767]=a;f[d+4>>2]=a|1;f[a+d>>2]=a;return}a=(c&-8)+a|0;g:{if(c>>>0<=255){b=f[g+8>>2];c=c>>>3|0;e=f[g+12>>2];if((b|0)==(e|0)){f[7765]=f[7765]&ib(-2,c);break g}f[b+12>>2]=e;f[e+8>>2]=b;break g}j=f[g+24>>2];c=f[g+12>>2];h:{if((g|0)!=(c|0)){b=f[g+8>>2];f[b+12>>2]=c;f[c+8>>2]=b;break h}i:{e=g+20|0;b=f[e>>2];if(b){break i}e=g+16|0;b=f[e>>2];if(b){break i}c=0;break h}while(1){h=e;c=b;e=c+20|0;b=f[e>>2];if(b){continue}e=c+16|0;b=f[c+16>>2];if(b){continue}break}f[h>>2]=0}if(!j){break g}e=f[g+28>>2];b=(e<<2)+31364|0;j:{if(f[b>>2]==(g|0)){f[b>>2]=c;if(c){break j}f[7766]=f[7766]&ib(-2,e);break g}f[j+(f[j+16>>2]==(g|0)?16:20)>>2]=c;if(!c){break g}}f[c+24>>2]=j;b=f[g+16>>2];if(b){f[c+16>>2]=b;f[b+24>>2]=c}b=f[g+20>>2];if(!b){break g}f[c+20>>2]=b;f[b+24>>2]=c}f[d+4>>2]=a|1;f[a+d>>2]=a;if(f[7770]!=(d|0)){break f}f[7767]=a;return}f[g+4>>2]=c&-2;f[d+4>>2]=a|1;f[a+d>>2]=a}if(a>>>0<=255){a=a>>>3|0;c=(a<<3)+31100|0;b=f[7765];a=1<<a;k:{if(!(b&a)){f[7765]=a|b;a=c;break k}a=f[c+8>>2]}f[c+8>>2]=d;f[a+12>>2]=d;f[d+12>>2]=c;f[d+8>>2]=a;return}f[d+16>>2]=0;f[d+20>>2]=0;g=d;e=a>>>8|0;b=0;l:{if(!e){break l}b=31;if(a>>>0>16777215){break l}c=e;e=e+1048320>>>16&8;b=c<<e;j=b+520192>>>16&4;b=b<<j;h=b+245760>>>16&2;b=(b<<h>>>15|0)-(h|(e|j))|0;b=(b<<1|a>>>b+21&1)+28|0}f[g+28>>2]=b;h=(b<<2)+31364|0;m:{n:{e=f[7766];c=1<<b;o:{if(!(e&c)){f[7766]=c|e;f[h>>2]=d;f[d+24>>2]=h;break o}e=a<<((b|0)==31?0:25-(b>>>1|0)|0);c=f[h>>2];while(1){b=c;if((f[c+4>>2]&-8)==(a|0)){break n}c=e>>>29|0;e=e<<1;h=b+(c&4)|0;c=f[h+16>>2];if(c){continue}break}f[h+16>>2]=d;f[d+24>>2]=b}f[d+12>>2]=d;f[d+8>>2]=d;break m}a=f[b+8>>2];f[a+12>>2]=d;f[b+8>>2]=d;f[d+24>>2]=0;f[d+12>>2]=b;f[d+8>>2]=a}a=f[7773]+ -1|0;f[7773]=a;if(a){break a}d=31516;while(1){a=f[d>>2];d=a+8|0;if(a){continue}break}f[7773]=-1}}function ea(a,b,c){var d=0,e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0;e=f[a+36>>2];i=f[a+32>>2];k=f[a+28>>2];j=f[a+24>>2];h=f[a+20>>2];if(c>>>0>=16){A=!g[a+76|0]<<24;p=f[a+4>>2];C=l(p,5);u=f[a+8>>2];z=l(u,5);w=f[a+12>>2];x=l(w,5);d=f[a+16>>2];v=l(d,5);D=d;q=f[a>>2];while(1){d=g[b+3|0];n=g[b+6|0];j=((d|g[b+4|0]<<8|g[b+5|0]<<16|n<<24)>>>2&67108863)+j|0;m=hb(j,0,w);o=B;y=m;r=(d<<24&50331648|(g[b|0]|g[b+1|0]<<8|g[b+2|0]<<16))+h|0;m=hb(r,0,D);h=y+m|0;d=B+o|0;d=h>>>0<m>>>0?d+1|0:d;m=n|g[b+7|0]<<8|g[b+8|0]<<16;n=g[b+9|0];o=((m|n<<24)>>>4&67108863)+k|0;m=hb(o,0,u);k=m+h|0;h=B+d|0;h=k>>>0<m>>>0?h+1|0:h;d=k;s=((n|g[b+10|0]<<8|g[b+11|0]<<16|g[b+12|0]<<24)>>>6|0)+i|0;k=hb(s,0,p);i=d+k|0;d=B+h|0;t=(g[b+13|0]|A|g[b+14|0]<<8|g[b+15|0]<<16)+e|0;h=hb(t,0,q);e=h+i|0;d=B+(i>>>0<k>>>0?d+1|0:d)|0;y=e;n=e>>>0<h>>>0?d+1|0:d;d=hb(j,0,u);e=B;h=hb(r,0,w);d=h+d|0;e=B+e|0;e=d>>>0<h>>>0?e+1|0:e;i=hb(o,0,p);h=i+d|0;d=B+e|0;d=h>>>0<i>>>0?d+1|0:d;i=hb(s,0,q);e=i+h|0;h=B+d|0;h=e>>>0<i>>>0?h+1|0:h;i=hb(t,0,v);e=i+e|0;d=B+h|0;E=e;m=e>>>0<i>>>0?d+1|0:d;d=hb(j,0,p);h=B;i=hb(r,0,u);e=i+d|0;d=B+h|0;d=e>>>0<i>>>0?d+1|0:d;i=hb(o,0,q);h=i+e|0;e=B+d|0;e=h>>>0<i>>>0?e+1|0:e;i=hb(s,0,v);h=i+h|0;d=B+e|0;d=h>>>0<i>>>0?d+1|0:d;i=hb(t,0,x);e=i+h|0;h=B+d|0;F=e;k=e>>>0<i>>>0?h+1|0:h;d=hb(j,0,q);h=B;i=hb(r,0,p);e=i+d|0;d=B+h|0;d=e>>>0<i>>>0?d+1|0:d;h=hb(o,0,v);e=h+e|0;d=B+d|0;d=e>>>0<h>>>0?d+1|0:d;i=hb(s,0,x);h=i+e|0;e=B+d|0;e=h>>>0<i>>>0?e+1|0:e;i=hb(t,0,z);h=i+h|0;d=B+e|0;G=h;i=h>>>0<i>>>0?d+1|0:d;d=hb(j,0,v);e=B;j=hb(r,0,q);d=j+d|0;h=B+e|0;h=d>>>0<j>>>0?h+1|0:h;j=hb(o,0,x);e=j+d|0;d=B+h|0;d=e>>>0<j>>>0?d+1|0:d;h=hb(s,0,z);e=h+e|0;d=B+d|0;d=e>>>0<h>>>0?d+1|0:d;j=hb(t,0,C);h=j+e|0;e=B+d|0;e=h>>>0<j>>>0?e+1|0:e;j=h;d=i;e=(e&67108863)<<6|h>>>26;h=e+G|0;if(h>>>0<e>>>0){d=d+1|0}i=h;h=k;d=(d&67108863)<<6|i>>>26;e=d+F|0;if(e>>>0<d>>>0){h=h+1|0}k=e;e=m;d=(h&67108863)<<6|k>>>26;h=d+E|0;if(h>>>0<d>>>0){e=e+1|0}d=n;e=(e&67108863)<<6|h>>>26;n=e+y|0;if(n>>>0<e>>>0){d=d+1|0}e=n;d=l((d&67108863)<<6|e>>>26,5)+(j&67108863)|0;j=(i&67108863)+(d>>>26|0)|0;k=k&67108863;i=h&67108863;e=e&67108863;h=d&67108863;b=b+16|0;c=c+ -16|0;if(c>>>0>15){continue}break}}f[a+36>>2]=e;f[a+32>>2]=i;f[a+28>>2]=k;f[a+24>>2]=j;f[a+20>>2]=h}function aa(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0;n=f[b+36>>2];e=f[b+32>>2];g=f[b+28>>2];h=f[b+24>>2];i=f[b+20>>2];j=f[b+16>>2];k=f[b+12>>2];m=f[b+8>>2];c=f[b+4>>2];b=f[b>>2];c=c+(b>>>26|0)|0;m=m+(c>>>25|0)|0;k=k+(m>>>26|0)|0;j=j+(k>>>25|0)|0;i=i+(j>>>26|0)|0;h=h+(i>>>25|0)|0;g=g+(h>>>26|0)|0;e=e+(g>>>25|0)|0;n=n+(e>>>26|0)|0;b=l(n>>>25|0,19)+(b&67108863)|0;o=b&67108863;b=(c&33554431)+(b>>>26|0)|0;c=(m&67108863)+(b>>>25|0)|0;m=(k&33554431)+(c>>>26|0)|0;k=(j&67108863)+(m>>>25|0)|0;j=(i&33554431)+(k>>>26|0)|0;i=(h&67108863)+(j>>>25|0)|0;h=(g&33554431)+(i>>>26|0)|0;g=(e&67108863)+(h>>>25|0)|0;e=(n&33554431)+(g>>>26|0)|0;n=(o+l(e>>>25|0,19)|0)+19|0;o=n&67108863;p=e&33554431;e=g&67108863;g=h&33554431;h=i&67108863;i=j&33554431;j=k&67108863;k=m&33554431;m=c&67108863;c=(b&33554431)+(n>>>26|0)|0;m=m+(c>>>25|0)|0;k=k+(m>>>26|0)|0;j=j+(k>>>25|0)|0;i=i+(j>>>26|0)|0;h=h+(i>>>25|0)|0;g=g+(h>>>26|0)|0;e=e+(g>>>25|0)|0;n=p+(e>>>26|0)|0;b=(o+l(n>>>25|0,19)|0)+67108845|0;d[a|0]=b;d[a+2|0]=b>>>16;d[a+1|0]=b>>>8;c=((c&33554431)+(b>>>26|0)|0)+33554431|0;d[a+5|0]=c>>>14;d[a+4|0]=c>>>6;d[a+3|0]=b>>>24&3|c<<2;b=((m&67108863)+(c>>>25|0)|0)+67108863|0;d[a+8|0]=b>>>13;d[a+7|0]=b>>>5;d[a+6|0]=c>>>22&7|b<<3;c=((k&33554431)+(b>>>26|0)|0)+33554431|0;d[a+11|0]=c>>>11;d[a+10|0]=c>>>3;d[a+9|0]=b>>>21&31|c<<5;b=((j&67108863)+(c>>>25|0)|0)+67108863|0;d[a+15|0]=b>>>18;d[a+14|0]=b>>>10;d[a+13|0]=b>>>2;d[a+12|0]=c>>>19&63|b<<6;b=((i&33554431)+(b>>>26|0)|0)+33554431|0;d[a+16|0]=b;d[a+18|0]=b>>>16;d[a+17|0]=b>>>8;c=((h&67108863)+(b>>>25|0)|0)+67108863|0;d[a+21|0]=c>>>15;d[a+20|0]=c>>>7;d[a+19|0]=b>>>24&1|c<<1;b=((g&33554431)+(c>>>26|0)|0)+33554431|0;d[a+24|0]=b>>>13;d[a+23|0]=b>>>5;d[a+22|0]=c>>>23&7|b<<3;c=((e&67108863)+(b>>>25|0)|0)+67108863|0;d[a+27|0]=c>>>12;d[a+26|0]=c>>>4;d[a+25|0]=b>>>21&15|c<<4;b=(n+(c>>>26|0)|0)+33554431|0;d[a+30|0]=b>>>10;d[a+29|0]=b>>>2;d[a+31|0]=b>>>18&127;d[a+28|0]=c>>>20&63|b<<6}function V(a,b,c){var d=0,e=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0,ea=0,fa=0,ga=0,ha=0,ia=0;d=A-112|0;A=d;O((d+48|0)+c|0,0,c>>>0>63?0:64-c|0);P(d+48|0,b,c);o=g[d+81|0];p=h[d+84>>1];q=g[d+83|0];r=g[d+82|0];s=h[d+88>>1];t=g[d+87|0];u=g[d+86|0];v=g[d+91|0];w=g[d+90|0];x=g[d+93|0];y=g[d+92|0];z=h[d+94>>1];B=g[d+96|0];C=g[d+100|0];D=g[d+99|0];E=g[d+98|0];F=g[d+97|0];G=g[d+104|0];H=g[d+103|0];I=g[d+102|0];J=g[d+101|0];K=g[d+108|0];L=g[d+107|0];M=g[d+106|0];N=g[d+105|0];Q=g[d+109|0]|g[d+110|0]<<8;R=g[d+111|0];S=g[d+54|0];T=g[d+53|0];U=g[d+52|0];V=g[d+58|0];W=g[d+57|0];X=g[d+56|0];b=g[d+55|0];Y=g[d+63|0];Z=h[d+64>>1];j=h[d+68>>1];_=g[d+67|0];$=g[d+66|0];e=h[d+72>>1];aa=g[d+71|0];ba=g[d+70|0];ca=g[d+62|0];da=g[d+61|0];ea=g[d+60|0];c=g[d+59|0];i=g[d+75|0];fa=g[d+74|0];ga=g[d+77|0];ha=g[d+76|0];k=h[d+48>>1];ia=g[d+50|0];l=g[d+51|0];m=g[d+79|0];n=g[d+80|0];f[a+32>>2]=g[d+78|0]|m<<8|n<<16;f[a>>2]=l<<24&1056964608|(ia<<16|k);k=i<<24;i=fa<<16;f[a+28>>2]=ga<<22|ha<<14|(k|i)>>>18;f[a+12>>2]=ea<<6|c>>>2|da<<14|ca<<22;i=(e|i)<<12&1073737728;e=ba<<16;f[a+24>>2]=i|(e|aa<<24)>>>20;e=(j|e)<<10&1073740800;j=$<<16;f[a+20>>2]=e|(j|_<<24)>>>22;f[a+16>>2]=(j|Z)<<8&1073741568|Y;f[a+8>>2]=c<<28&805306368|(X<<4|b>>>4|W<<12|V<<20);f[a+4>>2]=b<<26&1006632960|(U<<2|l>>>6|T<<10|S<<18);f[d+32>>2]=R<<16|Q;b=N<<8;f[d+28>>2]=K<<22|(b|M<<16|L<<24)>>>10;c=(b|G)<<20&1072693248;b=J<<8;f[d+24>>2]=c|(b|I<<16|H<<24)>>>12;c=(b|C)<<18&1073479680;b=F<<8;f[d+20>>2]=c|(b|E<<16|D<<24)>>>14;f[d+16>>2]=(b|B)<<16&1073676288|z;b=w<<16;f[d+12>>2]=x<<22|y<<14|(b|v<<24)>>>18;c=(b|s)<<12&1073737728;b=u<<16;f[d+8>>2]=c|(b|t<<24)>>>20;c=(b|p)<<10&1073740800;b=r<<16;f[d+4>>2]=c|(b|q<<24)>>>22;f[d>>2]=(b|(o<<8|n))<<8&1073741568|m;ra(a,d,a);A=d+112|0}function Oa(a,b,c){var e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0;e=A-96|0;A=e;V(e+48|0,a,32);V(e,b,32);a=f[e>>2]+f[e+48>>2]|0;b=a&1073741823;f[e+48>>2]=b;a=f[e+4>>2]+(f[e+52>>2]+(a>>>30|0)|0)|0;m=a&1073741823;f[e+52>>2]=m;a=f[e+8>>2]+(f[e+56>>2]+(a>>>30|0)|0)|0;j=a&1073741823;f[e+56>>2]=j;a=f[e+12>>2]+f[e+60>>2]+(a>>>30)|0;k=a&1073741823;f[e+60>>2]=k;a=f[e+16>>2]+f[e+64>>2]+(a>>>30)|0;i=f[e+20>>2]+f[e+68>>2]+(a>>>30)|0;g=f[e+24>>2]+f[e+72>>2]+(i>>>30)|0;h=f[e+28>>2]+f[e+76>>2]+(g>>>30)|0;l=f[e+32>>2]+f[e+80>>2]+(h>>>30)|0;h=h&1073741823;g=g&1073741823;i=i&1073741823;o=a&1073741823;n=b+ -485872621|0;p=(m+(n>>31)|0)+ -541690985|0;q=(j+(p>>31)|0)+ -796511589|0;r=q>>>31|0;s=k-(r|935229352)|0;t=s>>>31|0;u=o-(t|20)|0;v=u>>>31|0;w=i-v|0;x=w>>>31|0;y=g-x|0;z=y>>>31|0;B=h-z|0;C=B>>>31|0;D=l-(C|4096)|0;E=D>>>31|0;a=E+ -1|0;b=b^a&(b^n+(n>>>1&1073741824));f[e+48>>2]=b;n=l^a&(l^(E<<16)+D);f[e+80>>2]=n;l=h^a&(h^(C<<30)+B);f[e+76>>2]=l;h=g^a&(g^(z<<30)+y);f[e+72>>2]=h;g=i^a&(i^(x<<30)+w);f[e+68>>2]=g;i=o^a&(o^(v<<30)+u);f[e+64>>2]=i;k=k^a&(k^s+(t<<30));f[e+60>>2]=k;j=j^a&(j^q+(r<<30));f[e+56>>2]=j;a=m^a&(m^p+(p>>>1&1073741824));f[e+52>>2]=a;d[c+1|0]=b>>>8;d[c+2|0]=b>>>16;m=a>>>2|0;d[c+4|0]=m;d[c+5|0]=a>>>10;d[c+6|0]=a>>>18;o=j>>>4|0;d[c+8|0]=o;d[c+9|0]=j>>>12;d[c+10|0]=j>>>20;p=k>>>6|0;d[c+12|0]=p;d[c+13|0]=k>>>14;d[c+14|0]=k>>>22;q=i>>>8|0;d[c+16|0]=q;d[c+17|0]=i>>>16;d[c+19|0]=g>>>2;r=g>>>10|0;d[c+20|0]=r;d[c+21|0]=g>>>18;d[c+23|0]=h>>>4;s=h>>>12|0;d[c+24|0]=s;d[c+25|0]=h>>>20;d[c+27|0]=l>>>6;t=l>>>14|0;d[c+28|0]=t;d[c+29|0]=l>>>22;d[c|0]=b;d[c+3|0]=(b|a<<30)>>>24;d[c+7|0]=(m|j<<28)>>>24;d[c+11|0]=(o|k<<26)>>>24;d[c+15|0]=(p|i<<24)>>>24;d[c+18|0]=(q|g<<22)>>>16;d[c+22|0]=(r|h<<20)>>>16;d[c+26|0]=(s|l<<18)>>>16;d[c+31|0]=n>>>8;d[c+30|0]=(t|n<<16)>>>16;A=e+96|0}function Ha(a,b){var c=0;O(a- -64|0,0,176);c=f[7707];f[a+56>>2]=f[7706];f[a+60>>2]=c;c=f[7705];f[a+48>>2]=f[7704];f[a+52>>2]=c;c=f[7703];f[a+40>>2]=f[7702];f[a+44>>2]=c;c=f[7701];f[a+32>>2]=f[7700];f[a+36>>2]=c;c=f[7699];f[a+24>>2]=f[7698];f[a+28>>2]=c;c=f[7697];f[a+16>>2]=f[7696];f[a+20>>2]=c;c=f[7695];f[a+8>>2]=f[7694];f[a+12>>2]=c;c=f[7693];f[a>>2]=f[7692];f[a+4>>2]=c;c=(g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24))^1779033703;f[a>>2]=(g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24))^-205731576;f[a+4>>2]=c;c=(g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24))^-1150833019;f[a+8>>2]=(g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24))^-2067093701;f[a+12>>2]=c;c=f[a+20>>2]^(g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24));f[a+16>>2]=f[a+16>>2]^(g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24));f[a+20>>2]=c;c=f[a+28>>2]^(g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24));f[a+24>>2]=f[a+24>>2]^(g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24));f[a+28>>2]=c;c=f[a+36>>2]^(g[b+36|0]|g[b+37|0]<<8|(g[b+38|0]<<16|g[b+39|0]<<24));f[a+32>>2]=f[a+32>>2]^(g[b+32|0]|g[b+33|0]<<8|(g[b+34|0]<<16|g[b+35|0]<<24));f[a+36>>2]=c;c=f[a+44>>2]^(g[b+44|0]|g[b+45|0]<<8|(g[b+46|0]<<16|g[b+47|0]<<24));f[a+40>>2]=f[a+40>>2]^(g[b+40|0]|g[b+41|0]<<8|(g[b+42|0]<<16|g[b+43|0]<<24));f[a+44>>2]=c;c=f[a+52>>2]^(g[b+52|0]|g[b+53|0]<<8|(g[b+54|0]<<16|g[b+55|0]<<24));f[a+48>>2]=f[a+48>>2]^(g[b+48|0]|g[b+49|0]<<8|(g[b+50|0]<<16|g[b+51|0]<<24));f[a+52>>2]=c;c=f[a+60>>2]^(g[b+60|0]|g[b+61|0]<<8|(g[b+62|0]<<16|g[b+63|0]<<24));f[a+56>>2]=f[a+56>>2]^(g[b+56|0]|g[b+57|0]<<8|(g[b+58|0]<<16|g[b+59|0]<<24));f[a+60>>2]=c;f[a+228>>2]=g[b|0]}function Va(a,b,c,d,i,j){var k=0,l=0,m=0;k=A-256|0;A=k;a:{if(c){O(k- -64|0,0,131);l=h[529]|h[530]<<16;m=h[527]|h[528]<<16;e[k+14>>1]=m;e[k+16>>1]=m>>>16;e[k+18>>1]=l;e[k+20>>1]=l>>>16;l=f[263];f[k+8>>2]=f[262];f[k+12>>2]=l;l=f[261];f[k>>2]=f[260];f[k+4>>2]=l;X(b,c,k,k+208|0);Z(k- -64|0,k+208|0,k+240|0);f[k+232>>2]=0;f[k+236>>2]=0;f[k+224>>2]=0;f[k+228>>2]=0;f[k+240>>2]=0;f[k+244>>2]=0;f[k+216>>2]=0;f[k+220>>2]=0;f[k+208>>2]=0;f[k+212>>2]=0;Y(k,k- -64|0,a,64);O(k- -64|0,0,131);break a}b=g[a+60|0]|g[a+61|0]<<8|(g[a+62|0]<<16|g[a+63|0]<<24);f[k+56>>2]=g[a+56|0]|g[a+57|0]<<8|(g[a+58|0]<<16|g[a+59|0]<<24);f[k+60>>2]=b;b=g[a+52|0]|g[a+53|0]<<8|(g[a+54|0]<<16|g[a+55|0]<<24);f[k+48>>2]=g[a+48|0]|g[a+49|0]<<8|(g[a+50|0]<<16|g[a+51|0]<<24);f[k+52>>2]=b;b=g[a+44|0]|g[a+45|0]<<8|(g[a+46|0]<<16|g[a+47|0]<<24);f[k+40>>2]=g[a+40|0]|g[a+41|0]<<8|(g[a+42|0]<<16|g[a+43|0]<<24);f[k+44>>2]=b;b=g[a+36|0]|g[a+37|0]<<8|(g[a+38|0]<<16|g[a+39|0]<<24);f[k+32>>2]=g[a+32|0]|g[a+33|0]<<8|(g[a+34|0]<<16|g[a+35|0]<<24);f[k+36>>2]=b;b=g[a+28|0]|g[a+29|0]<<8|(g[a+30|0]<<16|g[a+31|0]<<24);f[k+24>>2]=g[a+24|0]|g[a+25|0]<<8|(g[a+26|0]<<16|g[a+27|0]<<24);f[k+28>>2]=b;b=g[a+20|0]|g[a+21|0]<<8|(g[a+22|0]<<16|g[a+23|0]<<24);f[k+16>>2]=g[a+16|0]|g[a+17|0]<<8|(g[a+18|0]<<16|g[a+19|0]<<24);f[k+20>>2]=b;b=g[a+4|0]|g[a+5|0]<<8|(g[a+6|0]<<16|g[a+7|0]<<24);f[k>>2]=g[a|0]|g[a+1|0]<<8|(g[a+2|0]<<16|g[a+3|0]<<24);f[k+4>>2]=b;b=g[a+12|0]|g[a+13|0]<<8|(g[a+14|0]<<16|g[a+15|0]<<24);f[k+8>>2]=g[a+8|0]|g[a+9|0]<<8|(g[a+10|0]<<16|g[a+11|0]<<24);f[k+12>>2]=b}ba(k,k- -64|0);Ra(d,i,k,k- -64|0,j);A=k+256|0}function na(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0;v=f[c>>2];j=v;w=f[c+4>>2];d=w;x=f[c+8>>2];k=x;y=f[c+12>>2];p=y;z=f[c+16>>2];l=z;A=f[c+20>>2];e=A;B=f[c+24>>2];m=B;C=f[c+28>>2];g=C;D=f[c+32>>2];h=D;E=f[c+36>>2];r=E;F=f[c+40>>2];n=F;G=f[c+44>>2];s=G;H=f[c+48>>2];q=H;I=f[c+52>>2];o=I;J=f[c+56>>2];t=J;K=f[c+60>>2];c=K;if((a|0)>=1){c=K;t=J;o=I;q=H;s=G;n=F;r=E;h=D;g=C;m=B;e=A;l=z;p=y;k=x;d=w;j=v;while(1){i=l;l=l+j|0;j=ib(l^q,16);h=j+h|0;q=ib(i^h,12);i=q;u=j;j=l+q|0;q=ib(u^j,8);h=q+h|0;l=ib(i^h,7);u=g;i=s;g=g+p|0;s=ib(g^c,16);i=i+s|0;c=ib(u^i,12);k=m+k|0;p=ib(k^t,16);n=p+n|0;m=ib(n^m,12);k=m+k|0;u=ib(k^p,8);L=c+g|0;p=L+l|0;g=ib(u^p,16);t=e;e=e+d|0;d=ib(e^o,16);r=d+r|0;M=ib(t^r,12);e=M+e|0;o=l;d=ib(e^d,8);N=d+r|0;l=g+N|0;o=ib(o^l,12);p=o+p|0;t=ib(g^p,8);r=t+l|0;l=ib(r^o,7);o=h;h=d;d=i;i=ib(s^L,8);d=d+i|0;g=ib(d^c,7);k=g+k|0;h=ib(h^k,16);c=o+h|0;g=ib(c^g,12);k=g+k|0;o=ib(h^k,8);h=c+o|0;g=ib(h^g,7);c=d;d=e;e=n+u|0;m=ib(e^m,7);d=d+m|0;n=ib(d^q,16);c=c+n|0;m=ib(c^m,12);d=m+d|0;q=ib(n^d,8);s=c+q|0;m=ib(s^m,7);c=ib(M^N,7);j=c+j|0;n=ib(j^i,16);e=n+e|0;i=ib(c^e,12);j=i+j|0;c=ib(n^j,8);n=e+c|0;e=ib(n^i,7);i=(a|0)>2;a=a+ -2|0;if(i){continue}break}}f[b+60>>2]=c+K;f[b+56>>2]=t+J;f[b+52>>2]=o+I;f[b+48>>2]=q+H;f[b+44>>2]=s+G;f[b+40>>2]=n+F;f[b+36>>2]=r+E;f[b+32>>2]=h+D;f[b+28>>2]=g+C;f[b+24>>2]=m+B;f[b+20>>2]=e+A;f[b+16>>2]=l+z;f[b+12>>2]=p+y;f[b+8>>2]=k+x;f[b+4>>2]=d+w;f[b>>2]=j+v}function pa(a,b,c){var e=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0;k=-1<<c+ -1;e=a;while(1){c=e;e=f[(h<<2)+b>>2];d[c|0]=e&1;d[c+29|0]=e>>>29&1;d[c+28|0]=e>>>28&1;d[c+27|0]=e>>>27&1;d[c+26|0]=e>>>26&1;d[c+25|0]=e>>>25&1;d[c+24|0]=e>>>24&1;d[c+23|0]=e>>>23&1;d[c+22|0]=e>>>22&1;d[c+21|0]=e>>>21&1;d[c+20|0]=e>>>20&1;d[c+19|0]=e>>>19&1;d[c+18|0]=e>>>18&1;d[c+17|0]=e>>>17&1;d[c+16|0]=e>>>16&1;d[c+15|0]=e>>>15&1;d[c+14|0]=e>>>14&1;d[c+13|0]=e>>>13&1;d[c+12|0]=e>>>12&1;d[c+11|0]=e>>>11&1;d[c+10|0]=e>>>10&1;d[c+9|0]=e>>>9&1;d[c+8|0]=e>>>8&1;e=e&255;d[c+7|0]=e>>>7;d[c+6|0]=e>>>6&1;d[c+5|0]=e>>>5&1;d[c+4|0]=e>>>4&1;d[c+3|0]=e>>>3&1;d[c+2|0]=e>>>2&1;d[c+1|0]=e>>>1&1;e=c+30|0;h=h+1|0;if((h|0)!=8){continue}break}b=f[b+32>>2];d[c+30|0]=b&1;d[c+45|0]=b>>>15&1;d[c+44|0]=b>>>14&1;d[c+43|0]=b>>>13&1;d[c+42|0]=b>>>12&1;d[c+41|0]=b>>>11&1;d[c+40|0]=b>>>10&1;d[c+39|0]=b>>>9&1;d[c+38|0]=b>>>8&1;b=b&255;d[c+37|0]=b>>>7;d[c+36|0]=b>>>6&1;d[c+35|0]=b>>>5&1;d[c+34|0]=b>>>4&1;d[c+33|0]=b>>>3&1;d[c+32|0]=b>>>2&1;d[c+31|0]=b>>>1&1;o=k^-1;while(1){a:{j=a+i|0;h=g[j|0];if(!h|i>>>0>254){break a}p=256-i|0;e=1;while(1){c=e+i|0;b=c+a|0;l=d[b|0];m=l<<e;h=h<<24>>24;n=m+h|0;b:{if((n|0)<=(o|0)){d[j|0]=n;d[b|0]=0;break b}b=h-m|0;if((b|0)>(k|0)){d[j|0]=b;if(c>>>0>255){break b}while(1){b=a+c|0;if(!g[b|0]){d[b|0]=1;break b}d[b|0]=0;b=c>>>0<255;c=c+1|0;if(b){continue}break}break b}if(l){break a}}b=e+1|0;if(e>>>0>5|b>>>0>=p>>>0){break a}h=g[j|0];e=b;continue}}i=i+1|0;if((i|0)!=256){continue}break}}function za(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0;e=f[a+56>>2];if(e){d[(a+e|0)+60|0]=1;c=e+1|0;if(c>>>0<=15){O((a+c|0)+60|0,0,15-e|0)}d[a+76|0]=1;ea(a,a+60|0,16)}u=f[a+52>>2];v=f[a+48>>2];n=f[a+44>>2];k=f[a+24>>2];h=f[a+28>>2]+(k>>>26|0)|0;j=f[a+32>>2]+(h>>>26|0)|0;p=f[a+36>>2]+(j>>>26|0)|0;c=f[a+20>>2]+l(p>>>26|0,5)|0;i=c&67108863;e=i+5|0;r=j&67108863;g=h&67108863;j=(k&67108863)+(c>>>26|0)|0;c=j+(e>>>26|0)|0;h=g+(c>>>26|0)|0;k=r+(h>>>26|0)|0;q=(p|-67108864)+(k>>>26|0)|0;m=q>>31;s=(q>>>31|0)+ -1|0;o=s&67108863;t=m&j|o&c;c=0;i=i&m|e&o|t<<26;j=i+f[a+40>>2]|0;if(j>>>0<i>>>0){c=1}i=j;d[b|0]=i;e=0;j=n;n=g&m|h&o;g=n<<20|t>>>6;h=j+g|0;if(h>>>0<g>>>0){e=1}j=h;h=c;g=j+c|0;c=e;c=g>>>0<h>>>0?c+1|0:c;h=g;d[b+4|0]=g;e=i;d[b+3|0]=e>>>24;d[b+2|0]=e>>>16;d[b+1|0]=e>>>8;e=0;k=m&r|k&o;g=k<<14|n>>>12;i=g+v|0;if(i>>>0<g>>>0){e=1}j=i;i=c;g=j+c|0;c=e;c=g>>>0<i>>>0?c+1|0:c;i=g;d[b+8|0]=g;e=h;d[b+7|0]=e>>>24;d[b+6|0]=e>>>16;d[b+5|0]=e>>>8;g=(q&s|m&p)<<8|k>>>18;h=g+u|0;h>>>0<g>>>0;h=c+h|0;c=h;d[b+12|0]=c;e=i;d[b+11|0]=e>>>24;d[b+10|0]=e>>>16;d[b+9|0]=e>>>8;d[b+15|0]=c>>>24;d[b+14|0]=c>>>16;d[b+13|0]=c>>>8;f[a+48>>2]=0;f[a+52>>2]=0;f[a+40>>2]=0;f[a+44>>2]=0;f[a+32>>2]=0;f[a+36>>2]=0;f[a+24>>2]=0;f[a+28>>2]=0;f[a+16>>2]=0;f[a+20>>2]=0;f[a+8>>2]=0;f[a+12>>2]=0;f[a>>2]=0;f[a+4>>2]=0}function P(a,b,c){var e=0,h=0,i=0;if(c>>>0>=512){y(a|0,b|0,c|0)|0;return a}h=a+c|0;a:{if(!((a^b)&3)){b:{if((c|0)<1){c=a;break b}if(!(a&3)){c=a;break b}c=a;while(1){d[c|0]=g[b|0];b=b+1|0;c=c+1|0;if(c>>>0>=h>>>0){break b}if(c&3){continue}break}}e=h&-4;c:{if(e>>>0<64){break c}i=e+ -64|0;if(c>>>0>i>>>0){break c}while(1){f[c>>2]=f[b>>2];f[c+4>>2]=f[b+4>>2];f[c+8>>2]=f[b+8>>2];f[c+12>>2]=f[b+12>>2];f[c+16>>2]=f[b+16>>2];f[c+20>>2]=f[b+20>>2];f[c+24>>2]=f[b+24>>2];f[c+28>>2]=f[b+28>>2];f[c+32>>2]=f[b+32>>2];f[c+36>>2]=f[b+36>>2];f[c+40>>2]=f[b+40>>2];f[c+44>>2]=f[b+44>>2];f[c+48>>2]=f[b+48>>2];f[c+52>>2]=f[b+52>>2];f[c+56>>2]=f[b+56>>2];f[c+60>>2]=f[b+60>>2];b=b- -64|0;c=c- -64|0;if(c>>>0<=i>>>0){continue}break}}if(c>>>0>=e>>>0){break a}while(1){f[c>>2]=f[b>>2];b=b+4|0;c=c+4|0;if(c>>>0<e>>>0){continue}break}break a}if(h>>>0<4){c=a;break a}e=h+ -4|0;if(e>>>0<a>>>0){c=a;break a}c=a;while(1){d[c|0]=g[b|0];d[c+1|0]=g[b+1|0];d[c+2|0]=g[b+2|0];d[c+3|0]=g[b+3|0];b=b+4|0;c=c+4|0;if(c>>>0<=e>>>0){continue}break}}if(c>>>0<h>>>0){while(1){d[c|0]=g[b|0];b=b+1|0;c=c+1|0;if((h|0)!=(c|0)){continue}break}}return a}function Ga(a,b,d){var e=0,h=0,j=0,k=0,l=0,m=0,n=0;e=A+ -64|0;A=e;f[e+56>>2]=0;f[e+60>>2]=0;f[e+48>>2]=0;f[e+52>>2]=0;f[e+40>>2]=0;f[e+44>>2]=0;f[e+32>>2]=0;f[e+36>>2]=0;f[e+24>>2]=0;f[e+28>>2]=0;f[e+16>>2]=0;f[e+20>>2]=0;f[e+8>>2]=0;f[e+12>>2]=0;f[e>>2]=0;f[e+4>>2]=0;if(!(!b|i[a+228>>2]>d>>>0|(f[a+80>>2]!=0|f[a+84>>2]!=0))){j=f[a+68>>2];h=j;n=f[a+64>>2];l=f[a+224>>2];k=l;m=n+k|0;if(m>>>0<k>>>0){h=h+1|0}d=a;f[a+64>>2]=m;f[a+68>>2]=h;k=f[a+76>>2];h=(h|0)==(j|0)&m>>>0<n>>>0|h>>>0<j>>>0;j=h+f[a+72>>2]|0;if(j>>>0<h>>>0){k=k+1|0}f[a+72>>2]=j;f[d+76>>2]=k;if(g[a+232|0]){f[a+88>>2]=-1;f[a+92>>2]=-1}f[a+80>>2]=-1;f[a+84>>2]=-1;d=a+96|0;O(l+d|0,0,128-l|0);fa(a,d);d=f[a+4>>2];f[e>>2]=f[a>>2];f[e+4>>2]=d;d=f[a+12>>2];f[e+8>>2]=f[a+8>>2];f[e+12>>2]=d;d=f[a+20>>2];f[e+16>>2]=f[a+16>>2];f[e+20>>2]=d;d=f[a+28>>2];f[e+24>>2]=f[a+24>>2];f[e+28>>2]=d;d=f[a+36>>2];f[e+32>>2]=f[a+32>>2];f[e+36>>2]=d;d=f[a+44>>2];f[e+40>>2]=f[a+40>>2];f[e+44>>2]=d;d=f[a+52>>2];f[e+48>>2]=f[a+48>>2];f[e+52>>2]=d;d=f[a+60>>2];f[e+56>>2]=f[a+56>>2];f[e+60>>2]=d;P(b,e,f[a+228>>2]);c[f[7708]](e,0,64)|0}A=e- -64|0}function Ea(a,b,c,e){var g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0;g=A-304|0;A=g;h=-1;if(!(!a|!c&(e|0)!=0|b+ -1>>>0>63)){f[g+260>>2]=0;f[g+264>>2]=0;f[g+268>>2]=0;f[g+272>>2]=0;f[g+276>>2]=0;f[g+280>>2]=0;f[g+284>>2]=0;f[g+288>>2]=0;f[g+292>>2]=0;f[g+296>>2]=0;f[g+300>>2]=0;f[g+244>>2]=0;f[g+248>>2]=0;d[g+243|0]=1;d[g+241|0]=256;d[g+242|0]=1;d[g+240|0]=b;f[g+252>>2]=0;f[g+256>>2]=0;Ha(g,g+240|0);if(e){h=f[g+224>>2];j=128-h|0;a:{if(j>>>0>=e>>>0){break a}f[g+224>>2]=0;l=g+96|0;P(l+h|0,c,j);m=f[g+68>>2];h=m;i=f[g+76>>2];m=f[g+64>>2];k=(h|0)==-1&m>>>0>4294967167;n=k+f[g+72>>2]|0;if(n>>>0<k>>>0){i=i+1|0}k=g;f[g+72>>2]=n;f[g+76>>2]=i;i=m+128|0;if(i>>>0<128){h=h+1|0}f[g+64>>2]=i;f[k+68>>2]=h;fa(g,l);c=c+j|0;e=e-j|0;if(e>>>0<129){break a}while(1){h=f[g+68>>2];j=h;i=f[g+64>>2];l=i+128|0;if(l>>>0<128){h=h+1|0}f[g+64>>2]=l;f[g+68>>2]=h;k=f[g+76>>2];j=(j|0)==-1&i>>>0>4294967167;i=j+f[g+72>>2]|0;if(i>>>0<j>>>0){k=k+1|0}f[g+72>>2]=i;f[g+76>>2]=k;fa(g,c);c=c+128|0;e=e+ -128|0;if(e>>>0>128){continue}break}}P((f[g+224>>2]+g|0)+96|0,c,e);f[g+224>>2]=f[g+224>>2]+e}Ga(g,a,b);h=0}A=g+304|0;return h}function Ba(a,b,c,d,e,h,i,j){var k=0,l=0,m=0,n=0,o=0;k=A-96|0;A=k;f[k+48>>2]=0;f[k+52>>2]=0;f[k+56>>2]=0;f[k+60>>2]=0;l=k- -64|0;f[l>>2]=0;f[l+4>>2]=0;f[k+72>>2]=0;f[k+76>>2]=0;f[k+80>>2]=0;f[k+84>>2]=0;f[k+88>>2]=0;f[k+92>>2]=0;f[k+32>>2]=0;f[k+36>>2]=0;f[k+40>>2]=0;f[k+44>>2]=0;f[k+12>>2]=1;f[a+48>>2]=0;f[a+52>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+56>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+60>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);la(a,k+32|0,k+32|0,64);a:{b:{if(j|!i){break b}ka(k+32|0,c,d,k+16|0);if((i|0)<1){break b}m=k+16|0;l=h;while(1){n=g[l|0]^g[m|0]|n;l=l+1|0;m=m+1|0;o=o+1|0;if((o|0)!=(i|0)){continue}break}l=-1;if(n){break a}}l=a;m=k+12|0;if(m){m=g[m|0]|g[m+1|0]<<8|(g[m+2|0]<<16|g[m+3|0]<<24)}else{m=0}f[l+48>>2]=m;f[a+52>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+56>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+60>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);la(a,c,e,d);if(!(!i|!j)){ka(k+32|0,e,d,k+16|0);P(h,k+16|0,i)}l=0}A=k+96|0;return l}function ma(a,b){d[a|0]=g[b|0]<<3;d[a+1|0]=g[b+1|0]<<3|g[b|0]>>>5;d[a+2|0]=g[b+2|0]<<3|g[b+1|0]>>>5;d[a+3|0]=g[b+3|0]<<3|g[b+2|0]>>>5;d[a+4|0]=g[b+4|0]<<3|g[b+3|0]>>>5;d[a+5|0]=g[b+5|0]<<3|g[b+4|0]>>>5;d[a+6|0]=g[b+6|0]<<3|g[b+5|0]>>>5;d[a+7|0]=g[b+7|0]<<3|g[b+6|0]>>>5;d[a+8|0]=g[b+8|0]<<3|g[b+7|0]>>>5;d[a+9|0]=g[b+9|0]<<3|g[b+8|0]>>>5;d[a+10|0]=g[b+10|0]<<3|g[b+9|0]>>>5;d[a+11|0]=g[b+11|0]<<3|g[b+10|0]>>>5;d[a+12|0]=g[b+12|0]<<3|g[b+11|0]>>>5;d[a+13|0]=g[b+13|0]<<3|g[b+12|0]>>>5;d[a+14|0]=g[b+14|0]<<3|g[b+13|0]>>>5;d[a+15|0]=g[b+15|0]<<3|g[b+14|0]>>>5;d[a+16|0]=g[b+16|0]<<3|g[b+15|0]>>>5;d[a+17|0]=g[b+17|0]<<3|g[b+16|0]>>>5;d[a+18|0]=g[b+18|0]<<3|g[b+17|0]>>>5;d[a+19|0]=g[b+19|0]<<3|g[b+18|0]>>>5;d[a+20|0]=g[b+20|0]<<3|g[b+19|0]>>>5;d[a+21|0]=g[b+21|0]<<3|g[b+20|0]>>>5;d[a+22|0]=g[b+22|0]<<3|g[b+21|0]>>>5;d[a+23|0]=g[b+23|0]<<3|g[b+22|0]>>>5;d[a+24|0]=g[b+24|0]<<3|g[b+23|0]>>>5;d[a+25|0]=g[b+25|0]<<3|g[b+24|0]>>>5;d[a+26|0]=g[b+26|0]<<3|g[b+25|0]>>>5;d[a+27|0]=g[b+27|0]<<3|g[b+26|0]>>>5;d[a+28|0]=g[b+27|0]>>>5}function Y(a,b,c,e){var h=0,i=0,j=0,k=0;j=A+ -64|0;A=j;a:{if(!e){break a}h=g[b+129|0];if(h){i=h>>>0<e>>>0?h:e;k=(i|0)>1?i:1;h=0;while(1){d[a+h|0]=g[((g[b+128|0]+h|0)+b|0)- -64|0]^g[c+h|0];h=h+1|0;if((k|0)!=(h|0)){continue}break}O((g[b+128|0]+b|0)- -64|0,0,i);d[b+129|0]=g[b+129|0]-i;d[b+128|0]=i+g[b+128|0];e=e-i|0;if(!e){break a}c=c+i|0;a=a+i|0}if(e>>>0>=64){while(1){na(g[b+130|0],j,b);h=f[b+48>>2];i=h+1|0;f[b+48>>2]=i;if(i>>>0<h>>>0){f[b+52>>2]=f[b+52>>2]+1}h=0;while(1){d[a+h|0]=g[h+j|0]^g[c+h|0];h=h+1|0;if((h|0)!=64){continue}break}a=a- -64|0;c=c- -64|0;e=e+ -64|0;if(e>>>0>63){continue}break}if(!e){break a}}na(g[b+130|0],j,b);h=f[b+48>>2];i=h+1|0;f[b+48>>2]=i;if(i>>>0<h>>>0){f[b+52>>2]=f[b+52>>2]+1}i=e>>>0>1?e:1;h=0;while(1){d[a+h|0]=g[h+j|0]^g[c+h|0];h=h+1|0;if((i|0)!=(h|0)){continue}break}d[b+128|0]=i;d[b+129|0]=64-e;if(i>>>0>63){break a}P((b+i|0)- -64|0,i+j|0,64-i|0)}A=j- -64|0}function oa(a){var b=0,c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,B=0;c=f[a+32>>2];d=f[a+28>>2];e=f[a+24>>2];g=f[a+20>>2];h=f[a+16>>2];i=f[a+12>>2];j=f[a+8>>2];k=f[a+4>>2];l=f[a>>2];m=l+ -485872621|0;n=(k+(m>>31)|0)+ -541690985|0;o=(j+(n>>31)|0)+ -796511589|0;p=o>>>31|0;q=i-(p|935229352)|0;r=q>>>31|0;s=h-(r|20)|0;t=s>>>31|0;u=g-t|0;v=u>>>31|0;w=e-v|0;x=w>>>31|0;y=d-x|0;z=y>>>31|0;A=c-(z|4096)|0;B=A>>>31|0;b=B+ -1|0;f[a+28>>2]=((z<<30)+y^d)&b^d;f[a+24>>2]=((x<<30)+w^e)&b^e;f[a+20>>2]=((v<<30)+u^g)&b^g;f[a+16>>2]=((t<<30)+s^h)&b^h;f[a+12>>2]=((r<<30)+q^i)&b^i;f[a+8>>2]=((p<<30)+o^j)&b^j;f[a+4>>2]=((n>>>1&1073741824)+n^k)&b^k;f[a>>2]=((m>>>1&1073741824)+m^l)&b^l;f[a+32>>2]=((B<<16)+A^c)&b^c}function La(a,b,c){f[a+12>>2]=1797285236;f[a>>2]=1634760805;f[a+8>>2]=2036477234;f[a+4>>2]=857760878;f[a+16>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+20>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+24>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[a+28>>2]=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[a+32>>2]=g[b+16|0]|g[b+17|0]<<8|g[b+18|0]<<16|g[b+19|0]<<24;f[a+36>>2]=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[a+40>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);b=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);f[a+48>>2]=0;f[a+44>>2]=b;f[a+52>>2]=0;f[a+56>>2]=g[c|0]|g[c+1|0]<<8|g[c+2|0]<<16|g[c+3|0]<<24;f[a+60>>2]=g[c+4|0]|g[c+5|0]<<8|g[c+6|0]<<16|g[c+7|0]<<24}function O(a,b,c){a=a|0;b=b|0;c=c|0;var e=0,g=0,h=0,i=0;a:{if(!c){break a}e=a+c|0;d[e+ -1|0]=b;d[a|0]=b;if(c>>>0<3){break a}d[e+ -2|0]=b;d[a+1|0]=b;d[e+ -3|0]=b;d[a+2|0]=b;if(c>>>0<7){break a}d[e+ -4|0]=b;d[a+3|0]=b;if(c>>>0<9){break a}e=0-a&3;g=e+a|0;b=l(b&255,16843009);f[g>>2]=b;c=c-e&-4;e=c+g|0;f[e+ -4>>2]=b;if(c>>>0<9){break a}f[g+8>>2]=b;f[g+4>>2]=b;f[e+ -8>>2]=b;f[e+ -12>>2]=b;if(c>>>0<25){break a}f[g+24>>2]=b;f[g+20>>2]=b;f[g+16>>2]=b;f[g+12>>2]=b;f[e+ -16>>2]=b;f[e+ -20>>2]=b;f[e+ -24>>2]=b;f[e+ -28>>2]=b;i=g&4|24;c=c-i|0;if(c>>>0<32){break a}e=b;h=b;b=g+i|0;while(1){f[b+24>>2]=h;f[b+28>>2]=e;f[b+16>>2]=h;f[b+20>>2]=e;f[b+8>>2]=h;f[b+12>>2]=e;f[b>>2]=h;f[b+4>>2]=e;b=b+32|0;c=c+ -32|0;if(c>>>0>31){continue}break}}return a|0}function Aa(a,b){var c=0,e=0;f[a>>2]=g[b+3|0]<<24&50331648|(g[b|0]|g[b+1|0]<<8|g[b+2|0]<<16);f[a+4>>2]=(g[b+3|0]|g[b+4|0]<<8|(g[b+5|0]<<16|g[b+6|0]<<24))>>>2&67108611;f[a+8>>2]=(g[b+6|0]|g[b+7|0]<<8|(g[b+8|0]<<16|g[b+9|0]<<24))>>>4&67092735;f[a+12>>2]=(g[b+9|0]|g[b+10|0]<<8|(g[b+11|0]<<16|g[b+12|0]<<24))>>>6&66076671;c=g[b+13|0]|g[b+14|0]<<8;e=g[b+15|0];f[a+20>>2]=0;f[a+24>>2]=0;f[a+28>>2]=0;f[a+32>>2]=0;f[a+36>>2]=0;f[a+16>>2]=e<<16&983040|c;f[a+40>>2]=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);f[a+44>>2]=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[a+48>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);b=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);d[a+76|0]=0;f[a+56>>2]=0;f[a+52>>2]=b}function ba(a,b){var c=0,d=0;c=A-272|0;A=c;f[c+56>>2]=0;f[c+60>>2]=0;f[c+48>>2]=0;f[c+52>>2]=0;f[c+40>>2]=0;f[c+44>>2]=0;f[c+32>>2]=0;f[c+36>>2]=0;d=g[a+20|0]|g[a+21|0]<<8|(g[a+22|0]<<16|g[a+23|0]<<24);f[c+16>>2]=g[a+16|0]|g[a+17|0]<<8|(g[a+18|0]<<16|g[a+19|0]<<24);f[c+20>>2]=d;d=g[a+28|0]|g[a+29|0]<<8|(g[a+30|0]<<16|g[a+31|0]<<24);f[c+24>>2]=g[a+24|0]|g[a+25|0]<<8|(g[a+26|0]<<16|g[a+27|0]<<24);f[c+28>>2]=d;d=g[a+4|0]|g[a+5|0]<<8|(g[a+6|0]<<16|g[a+7|0]<<24);f[c>>2]=g[a|0]|g[a+1|0]<<8|(g[a+2|0]<<16|g[a+3|0]<<24);f[c+4>>2]=d;d=g[a+12|0]|g[a+13|0]<<8|(g[a+14|0]<<16|g[a+15|0]<<24);f[c+8>>2]=g[a+8|0]|g[a+9|0]<<8|(g[a+10|0]<<16|g[a+11|0]<<24);f[c+12>>2]=d;V(c+224|0,c,32);sa(c- -64|0,c+224|0);da(b,c- -64|0);A=c+272|0}function ha(a,b){return(g[b|0]^g[a|0]|g[b+1|0]^g[a+1|0]|g[b+2|0]^g[a+2|0]|g[b+3|0]^g[a+3|0]|g[b+4|0]^g[a+4|0]|g[b+5|0]^g[a+5|0]|g[b+6|0]^g[a+6|0]|g[b+7|0]^g[a+7|0]|g[b+8|0]^g[a+8|0]|g[b+9|0]^g[a+9|0]|g[b+10|0]^g[a+10|0]|g[b+11|0]^g[a+11|0]|g[b+12|0]^g[a+12|0]|g[b+13|0]^g[a+13|0]|g[b+14|0]^g[a+14|0]|g[b+15|0]^g[a+15|0]|g[b+16|0]^g[a+16|0]|g[b+17|0]^g[a+17|0]|g[b+18|0]^g[a+18|0]|g[b+19|0]^g[a+19|0]|g[b+20|0]^g[a+20|0]|g[b+21|0]^g[a+21|0]|g[b+22|0]^g[a+22|0]|g[b+23|0]^g[a+23|0]|g[b+24|0]^g[a+24|0]|g[b+25|0]^g[a+25|0]|g[b+26|0]^g[a+26|0]|g[b+27|0]^g[a+27|0]|g[b+28|0]^g[a+28|0]|g[b+29|0]^g[a+29|0]|g[b+30|0]^g[a+30|0]|g[b+31|0]^g[a+31|0])+ -1>>>8&1}function N(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0;h=A-128|0;A=h;i=f[a+4>>2];e=i;j=f[a>>2];d=c+j|0;if(d>>>0<c>>>0){e=e+1|0}g=a;f[a>>2]=d;f[a+4>>2]=e;if((e|0)==(i|0)&d>>>0<j>>>0|e>>>0<i>>>0){d=f[a+12>>2];e=f[a+8>>2]+1|0;if(e>>>0<1){d=d+1|0}f[a+8>>2]=e;f[g+12>>2]=d}g=j&127;a:{if(!g){break a}d=128-g|0;if(d>>>0>c>>>0){k=g;break a}e=g;g=a+16|0;P(e+g|0,b,d);ga(a,g);b=b+d|0;c=c-d|0}b:{if(!(b&7)){if(c>>>0<=127){break b}while(1){ga(a,b);b=b+128|0;c=c+ -128|0;if(c>>>0>127){continue}break}break b}if(c>>>0<128){break b}while(1){ga(a,P(h,b,128));b=b+128|0;c=c+ -128|0;if(c>>>0>127){continue}break}}if(c){P((a+k|0)+16|0,b,c)}A=h+128|0}function Da(a,b){var c=0;f[a+16>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+20>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+24>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[a+28>>2]=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);c=g[b+16|0];b=b+16|0;f[a+32>>2]=c|g[b+1|0]<<8|g[b+2|0]<<16|g[b+3|0]<<24;f[a+36>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+40>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);b=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[a+12>>2]=1797285236;f[a+8>>2]=2036477234;f[a+4>>2]=857760878;f[a>>2]=1634760805;f[a+44>>2]=b}function Ka(a,b,c){var d=0,e=0,g=0,h=0;g=A-176|0;A=g;e=f[a+4>>2];d=f[a>>2];a:{b:{if((e|0)==(d|0)){T(a+8|0,a+208|0,e>>>3|0);break b}e=e-d|0;if(!d|e>>>0>c>>>0){break a}h=d;d=a+208|0;P(h+d|0,b,e);T(a+8|0,d,f[a+4>>2]>>>3|0);b=b+e|0;c=c-e|0}f[a>>2]=0}c:{if(!(b&7)){d=f[a+4>>2];if(c>>>0<d>>>0){break c}e=a+8|0;while(1){T(e,b,d>>>3|0);d=f[a+4>>2];b=d+b|0;c=c-d|0;if(c>>>0>=d>>>0){continue}break}break c}d=f[a+4>>2];if(c>>>0<d>>>0){break c}e=a+8|0;while(1){T(e,P(g,b,d),d>>>3|0);d=f[a+4>>2];b=d+b|0;c=c-d|0;if(c>>>0>=d>>>0){continue}break}}if(c){P((f[a>>2]+a|0)+208|0,b,c);f[a>>2]=f[a>>2]+c}A=g+176|0}function Za(a,b,c,d,g){a=a|0;b=b|0;c=c|0;d=d|0;g=g|0;var i=0,j=0,k=0,l=0;k=A-224|0;A=k;a:{if(b){i=O(k,0,131);l=h[529]|h[530]<<16;j=h[527]|h[528]<<16;e[i+206>>1]=j;e[i+208>>1]=j>>>16;e[i+210>>1]=l;e[i+212>>1]=l>>>16;j=f[263];f[i+200>>2]=f[262];f[i+204>>2]=j;j=f[261];f[i+192>>2]=f[260];f[i+196>>2]=j;X(a,b,i+192|0,i+144|0);Z(i,i+144|0,i+176|0);f[i+168>>2]=0;f[i+172>>2]=0;f[i+160>>2]=0;f[i+164>>2]=0;f[i+176>>2]=0;f[i+180>>2]=0;f[i+152>>2]=0;f[i+156>>2]=0;f[i+144>>2]=0;f[i+148>>2]=0;Y(d,i,c,g);O(i,0,131);break a}P(d,c,g)}A=k+224|0}function jb(a,b,c){var d=0,e=0,f=0,g=0;g=c&63;f=g;d=f&31;if(32<=f>>>0){d=-1>>>d|0}else{e=-1>>>d|0;d=(1<<d)-1<<32-d|-1>>>d}f=d&a;d=b&e;e=g&31;if(32<=g>>>0){d=f<<e;g=0}else{d=(1<<e)-1&f>>>32-e|d<<e;g=f<<e}f=d;e=0-c&63;d=e;c=d&31;if(32<=d>>>0){d=-1<<c;c=0}else{d=(1<<c)-1&-1>>>32-c|-1<<c;c=-1<<c}a=c&a;d=b&d;b=e&31;if(32<=e>>>0){c=0;a=d>>>b|0}else{c=d>>>b|0;a=((1<<b)-1&d)<<32-b|a>>>b}a=a|g;B=c|f;return a}



function R(a){a=O(a,0,144);f[a+200>>2]=327033209;f[a+204>>2]=1541459225;f[a+192>>2]=-79577749;f[a+196>>2]=528734635;f[a+184>>2]=725511199;f[a+188>>2]=-1694144372;f[a+176>>2]=-1377402159;f[a+180>>2]=1359893119;f[a+168>>2]=1595750129;f[a+172>>2]=-1521486534;f[a+160>>2]=-23791573;f[a+164>>2]=1013904242;f[a+152>>2]=-2067093701;f[a+156>>2]=-1150833019;f[a+144>>2]=-205731576;f[a+148>>2]=1779033703}function $(a,b,c){var e=0,h=0,i=0;a:{e=f[a+56>>2];if(e){h=16-e|0;h=h>>>0>c>>>0?c:h;if(h){while(1){d[((e+i|0)+a|0)+60|0]=g[b+i|0];e=f[a+56>>2];i=i+1|0;if((h|0)!=(i|0)){continue}break}}e=e+h|0;f[a+56>>2]=e;if(e>>>0<16){break a}ea(a,a+60|0,16);f[a+56>>2]=0;c=c-h|0;b=b+h|0}if(c>>>0>=16){e=c&-16;ea(a,b,e);c=c&15;b=b+e|0}if(!c){break a}P((f[a+56>>2]+a|0)+60|0,b,c);f[a+56>>2]=f[a+56>>2]+c}}function Wa(a,b,c){a=a|0;b=b|0;c=c|0;var e=0;e=A-384|0;A=e;a:{b:{if(c>>>0>=129){R(a);N(a,b,c);Q(a,e+256|0);c=64;break b}if((e+256|0)!=(b|0)){P(e+256|0,b,c)}if(c>>>0>127){break a}}O((e+256|0)+c|0,0,128-c|0)}c=0;while(1){b=g[(e+256|0)+c|0];d[(e+128|0)+c|0]=b^54;d[c+e|0]=b^92;c=c+1|0;if((c|0)!=128){continue}break}R(a);N(a,e+128|0,128);a=a+208|0;R(a);N(a,e,128);A=e+384|0}function Ya(a,b,c,d,e,g,h,i){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;g=g|0;h=h|0;i=i|0;var j=0;j=A+ -64|0;A=j;f[j+56>>2]=0;f[j+60>>2]=0;f[j+48>>2]=0;f[j+52>>2]=0;f[j+40>>2]=0;f[j+44>>2]=0;f[j+32>>2]=0;f[j+36>>2]=0;f[j+24>>2]=0;f[j+28>>2]=0;f[j+16>>2]=0;f[j+20>>2]=0;f[j+8>>2]=0;f[j+12>>2]=0;f[j>>2]=0;f[j+4>>2]=0;Ca(j,a);a=Ba(j,b,c,d,e,g,h,i);A=j- -64|0;return a|0}function da(a,b){var c=0,e=0;c=A-192|0;A=c;e=b+80|0;S(c+144|0,e,1);S(c+96|0,c+144|0,2);M(c+48|0,c+96|0,e);M(c+144|0,c+48|0,c+144|0);S(c+96|0,c+144|0,1);M(c+48|0,c+96|0,c+48|0);qa(c+48|0);S(c+48|0,c+48|0,5);M(c,c+48|0,c+144|0);M(c+144|0,b,c);M(c+96|0,b+40|0,c);aa(a,c+96|0);aa(c+48|0,c+144|0);d[a+31|0]=g[a+31|0]^g[c+48|0]<<7;A=c+192|0}function ka(a,b,c,d){var e=0;e=A-112|0;A=e;Aa(e+32|0,a);f[e+8>>2]=0;f[e+12>>2]=0;f[e>>2]=0;f[e+4>>2]=0;$(e+32|0,0,0);$(e+32|0,b,c);a=(c|0)%16|0;if(a){$(e+32|0,e,16-a|0)}f[e+24>>2]=0;f[e+28>>2]=0;$(e+32|0,e+24|0,8);f[e+24>>2]=c;f[e+28>>2]=c>>31;$(e+32|0,e+24|0,8);za(e+32|0,d);A=e+112|0}function qa(a){var b=0;b=A-96|0;A=b;S(b+48|0,a,5);M(a,b+48|0,a);S(b+48|0,a,10);M(b,b+48|0,a);S(b+48|0,b,20);M(b+48|0,b+48|0,b);S(b+48|0,b+48|0,10);M(a,b+48|0,a);S(b+48|0,a,50);M(b,b+48|0,a);S(b+48|0,b,100);M(b+48|0,b+48|0,b);S(b+48|0,b+48|0,50);M(a,b+48|0,a);A=b+96|0}function Ia(a,b){var c=0,e=0;c=f[a>>2];if((c|0)==f[a+4>>2]){T(a+8|0,a+208|0,c>>>3|0);f[a>>2]=0;c=0}f[a>>2]=c+1;e=c;c=a+208|0;d[e+c|0]=6;e=f[a>>2];O(e+c|0,0,f[a+4>>2]-e|0);e=(f[a+4>>2]+c|0)+ -1|0;d[e|0]=g[e|0]|128;T(a+8|0,c,f[a+4>>2]>>>3|0);f[a>>2]=0;Ja(a,b)}function Ca(a,b){f[a>>2]=0;f[a+4>>2]=0;f[a+56>>2]=0;f[a+60>>2]=0;f[a+48>>2]=0;f[a+52>>2]=0;f[a+40>>2]=0;f[a+44>>2]=0;f[a+32>>2]=0;f[a+36>>2]=0;f[a+24>>2]=0;f[a+28>>2]=0;f[a+16>>2]=0;f[a+20>>2]=0;f[a+8>>2]=0;f[a+12>>2]=0;Da(a,b)}function gb(a,b,c){var d=0,e=0,f=0,g=0,h=0;e=c>>>16|0;d=a>>>16|0;h=l(e,d);f=c&65535;a=a&65535;g=l(f,a);d=(g>>>16|0)+l(d,f)|0;a=(d&65535)+l(a,e)|0;B=h+l(b,c)+(d>>>16)+(a>>>16)|0;return g&65535|a<<16}function W(a){var b=0,c=0;b=f[7892];c=a+3&-4;a=b+c|0;a:{if(a>>>0<=b>>>0?(c|0)>=1:0){break a}if(a>>>0>C()<<16>>>0){if(!(x(a|0)|0)){break a}}f[7892]=a;return b}f[7764]=48;return-1}function Ma(a,b){var c=0;c=A-208|0;A=c;R(c);N(c,a,32);Q(c,b);d[b|0]=g[b|0]&248;a=g[b+31|0];d[b+31|0]=a&63|64;A=c+208|0;return a>>>5&1}function _a(a,b,c){a=a|0;b=b|0;c=c|0;var d=0;d=A-208|0;A=d;d=O(d,0,208);f[O(d,0,344)+4>>2]=136;Ka(d,a,b);Ia(d,c);A=d+208|0}function ib(a,b){var c=0,d=0;c=b&31;d=(-1>>>c&a)<<c;c=a;a=0-b&31;return d|(c&-1<<a)>>>a}function ab(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;return Fa(a,b,c,d,e,f)|0}function ya(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;Va(c,a,b,d,e,f)}function cb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;Pa(c,a,b,d,e,f)}function bb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;Sa(a,b,c,d,e,f)}function db(a,b,c,d,e){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;return eb(a,b,c,d,e)|0}function va(a,b,c,d){a=a|0;b=b|0;c=c|0;d=d|0;return Qa(a,b,c,d)|0}function $a(a,b,c,d){a=a|0;b=b|0;c=c|0;d=d|0;return Ea(c,d,a,b)|0}function Ta(a,b){a=a|0;b=b|0;Q(a,b);a=a+208|0;N(a,b,64);Q(a,b)}function Z(a,b,c){a=O(a,0,136);d[a+130|0]=20;La(a,b,c)}function Ua(a,b,c){a=a|0;b=b|0;c=c|0;N(a,b,c)}function ta(a,b){a=a|0;b=b|0;ba(a,b)}function hb(a,b,c){return gb(a,b,c)}function ua(a){a=a|0;return u()|0}function Xa(){return 416}function fb(){}
// EMSCRIPTEN_END_FUNCS
c[1]=O;function C(){return buffer.byteLength/65536|0}return{"d":fb,"e":ya,"f":va,"g":ta,"h":db,"i":cb,"j":bb,"k":ab,"l":$a,"m":_a,"n":Za,"o":Ya,"p":Xa,"q":Wa,"r":Ua,"s":Ta,"t":xa,"u":wa,"v":ua}}var D=new Uint8Array(wasmMemory.buffer);for(var E=new Uint8Array(123),F=25;F>=0;--F){E[48+F]=52+F;E[65+F]=F;E[97+F]=26+F}E[43]=62;E[47]=63;function G(uint8Array,offset,b64){var H,I,F=0,J=offset,K=b64.length,L=offset+(K*3>>2)-(b64[K-2]=="=")-(b64[K-1]=="=");for(;F<K;F+=4){H=E[b64.charCodeAt(F+1)];I=E[b64.charCodeAt(F+2)];uint8Array[J++]=E[b64.charCodeAt(F)]<<2|H>>4;if(J<L)uint8Array[J++]=H<<4|I>>2;if(J<L)uint8Array[J++]=I<<6|E[b64.charCodeAt(F+3)]}}G(D,1026,"AgABAAM=");G(D,1040,"ZW5jcnlwdGVkIHdhbGxldCBzYWx0");G(D,1072,"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");G(D,25728,"sKAOAtLJhgGdGI8Af2k1AGAMvQCn1/sBnkyAAmll4QEd/AQAkgyu");G(D,25776,"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");G(D,30928,"gA==");return b({"Int8Array":Int8Array,"Int16Array":Int16Array,"Int32Array":Int32Array,"Uint8Array":Uint8Array,"Uint16Array":Uint16Array,"Uint32Array":Uint32Array,"Float32Array":Float32Array,"Float64Array":Float64Array,"NaN":NaN,"Infinity":Infinity,"Math":Math},asmLibraryArg,wasmMemory.buffer)}


// EMSCRIPTEN_END_ASM




)(asmLibraryArg,wasmMemory,wasmTable)},instantiate:function(binary,info){return{then:function(ok){var module=new WebAssembly.Module(binary);ok({"instance":new WebAssembly.Instance(module)})}}},RuntimeError:Error};wasmBinary=[];if(typeof WebAssembly!=="object"){abort("no native wasm support detected")}var wasmMemory;var wasmTable=new WebAssembly.Table({"initial":2,"maximum":2,"element":"anyfunc"});var ABORT=false;var EXITSTATUS=0;function assert(condition,text){if(!condition){abort("Assertion failed: "+text)}}var UTF8Decoder=typeof TextDecoder!=="undefined"?new TextDecoder("utf8"):undefined;function UTF8ArrayToString(heap,idx,maxBytesToRead){var endIdx=idx+maxBytesToRead;var endPtr=idx;while(heap[endPtr]&&!(endPtr>=endIdx))++endPtr;if(endPtr-idx>16&&heap.subarray&&UTF8Decoder){return UTF8Decoder.decode(heap.subarray(idx,endPtr))}else{var str="";while(idx<endPtr){var u0=heap[idx++];if(!(u0&128)){str+=String.fromCharCode(u0);continue}var u1=heap[idx++]&63;if((u0&224)==192){str+=String.fromCharCode((u0&31)<<6|u1);continue}var u2=heap[idx++]&63;if((u0&240)==224){u0=(u0&15)<<12|u1<<6|u2}else{u0=(u0&7)<<18|u1<<12|u2<<6|heap[idx++]&63}if(u0<65536){str+=String.fromCharCode(u0)}else{var ch=u0-65536;str+=String.fromCharCode(55296|ch>>10,56320|ch&1023)}}}return str}function UTF8ToString(ptr,maxBytesToRead){return ptr?UTF8ArrayToString(HEAPU8,ptr,maxBytesToRead):""}var WASM_PAGE_SIZE=65536;var buffer,HEAP8,HEAPU8,HEAP16,HEAPU16,HEAP32,HEAPU32,HEAPF32,HEAPF64;function updateGlobalBufferAndViews(buf){buffer=buf;Module["HEAP8"]=HEAP8=new Int8Array(buf);Module["HEAP16"]=HEAP16=new Int16Array(buf);Module["HEAP32"]=HEAP32=new Int32Array(buf);Module["HEAPU8"]=HEAPU8=new Uint8Array(buf);Module["HEAPU16"]=HEAPU16=new Uint16Array(buf);Module["HEAPU32"]=HEAPU32=new Uint32Array(buf);Module["HEAPF32"]=HEAPF32=new Float32Array(buf);Module["HEAPF64"]=HEAPF64=new Float64Array(buf)}var DYNAMIC_BASE=5274608,DYNAMICTOP_PTR=31568;var INITIAL_INITIAL_MEMORY=Module["INITIAL_MEMORY"]||16777216;if(Module["wasmMemory"]){wasmMemory=Module["wasmMemory"]}else{wasmMemory=new WebAssembly.Memory({"initial":INITIAL_INITIAL_MEMORY/WASM_PAGE_SIZE,"maximum":INITIAL_INITIAL_MEMORY/WASM_PAGE_SIZE})}if(wasmMemory){buffer=wasmMemory.buffer}INITIAL_INITIAL_MEMORY=buffer.byteLength;updateGlobalBufferAndViews(buffer);HEAP32[DYNAMICTOP_PTR>>2]=DYNAMIC_BASE;function callRuntimeCallbacks(callbacks){while(callbacks.length>0){var callback=callbacks.shift();if(typeof callback=="function"){callback(Module);continue}var func=callback.func;if(typeof func==="number"){if(callback.arg===undefined){Module["dynCall_v"](func)}else{Module["dynCall_vi"](func,callback.arg)}}else{func(callback.arg===undefined?null:callback.arg)}}}var __ATPRERUN__=[];var __ATINIT__=[];var __ATMAIN__=[];var __ATPOSTRUN__=[];var runtimeInitialized=false;function preRun(){if(Module["preRun"]){if(typeof Module["preRun"]=="function")Module["preRun"]=[Module["preRun"]];while(Module["preRun"].length){addOnPreRun(Module["preRun"].shift())}}callRuntimeCallbacks(__ATPRERUN__)}function initRuntime(){runtimeInitialized=true;callRuntimeCallbacks(__ATINIT__)}function preMain(){callRuntimeCallbacks(__ATMAIN__)}function postRun(){if(Module["postRun"]){if(typeof Module["postRun"]=="function")Module["postRun"]=[Module["postRun"]];while(Module["postRun"].length){addOnPostRun(Module["postRun"].shift())}}callRuntimeCallbacks(__ATPOSTRUN__)}function addOnPreRun(cb){__ATPRERUN__.unshift(cb)}function addOnPostRun(cb){__ATPOSTRUN__.unshift(cb)}var runDependencies=0;var runDependencyWatcher=null;var dependenciesFulfilled=null;function addRunDependency(id){runDependencies++;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}}function removeRunDependency(id){runDependencies--;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}if(runDependencies==0){if(runDependencyWatcher!==null){clearInterval(runDependencyWatcher);runDependencyWatcher=null}if(dependenciesFulfilled){var callback=dependenciesFulfilled;dependenciesFulfilled=null;callback()}}}Module["preloadedImages"]={};Module["preloadedAudios"]={};function abort(what){if(Module["onAbort"]){Module["onAbort"](what)}what+="";err(what);ABORT=true;EXITSTATUS=1;what="abort("+what+"). Build with -s ASSERTIONS=1 for more info.";var e=new WebAssembly.RuntimeError(what);throw e}function hasPrefix(str,prefix){return String.prototype.startsWith?str.startsWith(prefix):str.indexOf(prefix)===0}var dataURIPrefix="data:application/octet-stream;base64,";function isDataURI(filename){return hasPrefix(filename,dataURIPrefix)}var fileURIPrefix="file://";function isFileURI(filename){return hasPrefix(filename,fileURIPrefix)}var wasmBinaryFile="lib.wasm";if(!isDataURI(wasmBinaryFile)){wasmBinaryFile=locateFile(wasmBinaryFile)}function getBinary(){try{if(wasmBinary){return new Uint8Array(wasmBinary)}var binary=tryParseAsDataURI(wasmBinaryFile);if(binary){return binary}if(readBinary){return readBinary(wasmBinaryFile)}else{throw"both async and sync fetching of the wasm failed"}}catch(err){abort(err)}}function getBinaryPromise(){if(!wasmBinary&&(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER)&&typeof fetch==="function"&&!isFileURI(wasmBinaryFile)){return fetch(wasmBinaryFile,{credentials:"same-origin"}).then(function(response){if(!response["ok"]){throw"failed to load wasm binary file at '"+wasmBinaryFile+"'"}return response["arrayBuffer"]()}).catch(function(){return getBinary()})}return Promise.resolve().then(getBinary)}function createWasm(){var info={"a":asmLibraryArg};function receiveInstance(instance,module){var exports=instance.exports;Module["asm"]=exports;removeRunDependency("wasm-instantiate")}addRunDependency("wasm-instantiate");function receiveInstantiatedSource(output){receiveInstance(output["instance"])}function instantiateArrayBuffer(receiver){return getBinaryPromise().then(function(binary){return WebAssembly.instantiate(binary,info)}).then(receiver,function(reason){err("failed to asynchronously prepare wasm: "+reason);abort(reason)})}function instantiateAsync(){if(!wasmBinary&&typeof WebAssembly.instantiateStreaming==="function"&&!isDataURI(wasmBinaryFile)&&!isFileURI(wasmBinaryFile)&&typeof fetch==="function"){fetch(wasmBinaryFile,{credentials:"same-origin"}).then(function(response){var result=WebAssembly.instantiateStreaming(response,info);return result.then(receiveInstantiatedSource,function(reason){err("wasm streaming compile failed: "+reason);err("falling back to ArrayBuffer instantiation");return instantiateArrayBuffer(receiveInstantiatedSource)})})}else{return instantiateArrayBuffer(receiveInstantiatedSource)}}if(Module["instantiateWasm"]){try{var exports=Module["instantiateWasm"](info,receiveInstance);return exports}catch(e){err("Module.instantiateWasm callback failed with error: "+e);return false}}instantiateAsync();return{}}__ATINIT__.push({func:function(){___wasm_call_ctors()}});function ___assert_fail(condition,filename,line,func){abort("Assertion failed: "+UTF8ToString(condition)+", at: "+[filename?UTF8ToString(filename):"unknown filename",line,func?UTF8ToString(func):"unknown function"])}function _emscripten_memcpy_big(dest,src,num){HEAPU8.copyWithin(dest,src,src+num)}function abortOnCannotGrowMemory(requestedSize){abort("OOM")}function _emscripten_resize_heap(requestedSize){requestedSize=requestedSize>>>0;abortOnCannotGrowMemory(requestedSize)}var ASSERTIONS=false;function intArrayToString(array){var ret=[];for(var i=0;i<array.length;i++){var chr=array[i];if(chr>255){if(ASSERTIONS){assert(false,"Character code "+chr+" ("+String.fromCharCode(chr)+")  at offset "+i+" not in 0x00-0xFF.")}chr&=255}ret.push(String.fromCharCode(chr))}return ret.join("")}var decodeBase64=typeof atob==="function"?atob:function(input){var keyStr="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";var output="";var chr1,chr2,chr3;var enc1,enc2,enc3,enc4;var i=0;input=input.replace(/[^A-Za-z0-9\+\/\=]/g,"");do{enc1=keyStr.indexOf(input.charAt(i++));enc2=keyStr.indexOf(input.charAt(i++));enc3=keyStr.indexOf(input.charAt(i++));enc4=keyStr.indexOf(input.charAt(i++));chr1=enc1<<2|enc2>>4;chr2=(enc2&15)<<4|enc3>>2;chr3=(enc3&3)<<6|enc4;output=output+String.fromCharCode(chr1);if(enc3!==64){output=output+String.fromCharCode(chr2)}if(enc4!==64){output=output+String.fromCharCode(chr3)}}while(i<input.length);return output};function intArrayFromBase64(s){if(typeof ENVIRONMENT_IS_NODE==="boolean"&&ENVIRONMENT_IS_NODE){var buf;try{buf=Buffer.from(s,"base64")}catch(_){buf=new Buffer(s,"base64")}return new Uint8Array(buf["buffer"],buf["byteOffset"],buf["byteLength"])}try{var decoded=decodeBase64(s);var bytes=new Uint8Array(decoded.length);for(var i=0;i<decoded.length;++i){bytes[i]=decoded.charCodeAt(i)}return bytes}catch(_){throw new Error("Converting base64 string to bytes failed.")}}function tryParseAsDataURI(filename){if(!isDataURI(filename)){return}return intArrayFromBase64(filename.slice(dataURIPrefix.length))}var asmLibraryArg={"c":___assert_fail,"b":_emscripten_memcpy_big,"a":_emscripten_resize_heap,"memory":wasmMemory,"table":wasmTable};var asm=createWasm();var ___wasm_call_ctors=Module["___wasm_call_ctors"]=function(){return(___wasm_call_ctors=Module["___wasm_call_ctors"]=Module["asm"]["d"]).apply(null,arguments)};var _emscripten_sign=Module["_emscripten_sign"]=function(){return(_emscripten_sign=Module["_emscripten_sign"]=Module["asm"]["e"]).apply(null,arguments)};var _emscripten_verify=Module["_emscripten_verify"]=function(){return(_emscripten_verify=Module["_emscripten_verify"]=Module["asm"]["f"]).apply(null,arguments)};var _emscripten_to_public=Module["_emscripten_to_public"]=function(){return(_emscripten_to_public=Module["_emscripten_to_public"]=Module["asm"]["g"]).apply(null,arguments)};var _emscripten_wallet_secret_from_seed=Module["_emscripten_wallet_secret_from_seed"]=function(){return(_emscripten_wallet_secret_from_seed=Module["_emscripten_wallet_secret_from_seed"]=Module["asm"]["h"]).apply(null,arguments)};var _emscripten_derive_private=Module["_emscripten_derive_private"]=function(){return(_emscripten_derive_private=Module["_emscripten_derive_private"]=Module["asm"]["i"]).apply(null,arguments)};var _emscripten_wallet_change_pass=Module["_emscripten_wallet_change_pass"]=function(){return(_emscripten_wallet_change_pass=Module["_emscripten_wallet_change_pass"]=Module["asm"]["j"]).apply(null,arguments)};var _emscripten_derive_public=Module["_emscripten_derive_public"]=function(){return(_emscripten_derive_public=Module["_emscripten_derive_public"]=Module["asm"]["k"]).apply(null,arguments)};var _emscripten_blake2b=Module["_emscripten_blake2b"]=function(){return(_emscripten_blake2b=Module["_emscripten_blake2b"]=Module["asm"]["l"]).apply(null,arguments)};var _emscripten_sha3_256=Module["_emscripten_sha3_256"]=function(){return(_emscripten_sha3_256=Module["_emscripten_sha3_256"]=Module["asm"]["m"]).apply(null,arguments)};var _emscripten_cardano_memory_combine=Module["_emscripten_cardano_memory_combine"]=function(){return(_emscripten_cardano_memory_combine=Module["_emscripten_cardano_memory_combine"]=Module["asm"]["n"]).apply(null,arguments)};var _emscripten_chacha20poly1305_enc=Module["_emscripten_chacha20poly1305_enc"]=function(){return(_emscripten_chacha20poly1305_enc=Module["_emscripten_chacha20poly1305_enc"]=Module["asm"]["o"]).apply(null,arguments)};var _emscripten_size_of_hmac_sha512_ctx=Module["_emscripten_size_of_hmac_sha512_ctx"]=function(){return(_emscripten_size_of_hmac_sha512_ctx=Module["_emscripten_size_of_hmac_sha512_ctx"]=Module["asm"]["p"]).apply(null,arguments)};var _emscripten_hmac_sha512_init=Module["_emscripten_hmac_sha512_init"]=function(){return(_emscripten_hmac_sha512_init=Module["_emscripten_hmac_sha512_init"]=Module["asm"]["q"]).apply(null,arguments)};var _emscripten_hmac_sha512_update=Module["_emscripten_hmac_sha512_update"]=function(){return(_emscripten_hmac_sha512_update=Module["_emscripten_hmac_sha512_update"]=Module["asm"]["r"]).apply(null,arguments)};var _emscripten_hmac_sha512_final=Module["_emscripten_hmac_sha512_final"]=function(){return(_emscripten_hmac_sha512_final=Module["_emscripten_hmac_sha512_final"]=Module["asm"]["s"]).apply(null,arguments)};var _malloc=Module["_malloc"]=function(){return(_malloc=Module["_malloc"]=Module["asm"]["t"]).apply(null,arguments)};var _free=Module["_free"]=function(){return(_free=Module["_free"]=Module["asm"]["u"]).apply(null,arguments)};var __growWasmMemory=Module["__growWasmMemory"]=function(){return(__growWasmMemory=Module["__growWasmMemory"]=Module["asm"]["v"]).apply(null,arguments)};var calledRun;dependenciesFulfilled=function runCaller(){if(!calledRun)run();if(!calledRun)dependenciesFulfilled=runCaller};function run(args){args=args||arguments_;if(runDependencies>0){return}preRun();if(runDependencies>0)return;function doRun(){if(calledRun)return;calledRun=true;Module["calledRun"]=true;if(ABORT)return;initRuntime();preMain();if(Module["onRuntimeInitialized"])Module["onRuntimeInitialized"]();postRun()}if(Module["setStatus"]){Module["setStatus"]("Running...");setTimeout(function(){setTimeout(function(){Module["setStatus"]("")},1);doRun()},1)}else{doRun()}}Module["run"]=run;if(Module["preInit"]){if(typeof Module["preInit"]=="function")Module["preInit"]=[Module["preInit"]];while(Module["preInit"].length>0){Module["preInit"].pop()()}}noExitRuntime=true;run();
if (typeof module !== "undefined") {  module["exports"] = Module; }
}).call(this,require('_process'),require("buffer").Buffer,"/")
},{"_process":46,"buffer":37,"fs":34,"path":44}],7:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// browserify by default only pulls in files that are hard coded in requires
// In order of last to first in this file, the default wordlist will be chosen
// based on what is present. (Bundles may remove wordlists they don't need)
const wordlists = {};
exports.wordlists = wordlists;
let _default;
exports._default = _default;
try {
    exports._default = _default = require('./wordlists/chinese_simplified.json');
    wordlists.chinese_simplified = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/chinese_traditional.json');
    wordlists.chinese_traditional = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/korean.json');
    wordlists.korean = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/french.json');
    wordlists.french = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/italian.json');
    wordlists.italian = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/spanish.json');
    wordlists.spanish = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/japanese.json');
    wordlists.japanese = _default;
    wordlists.JA = _default;
}
catch (err) { }
try {
    exports._default = _default = require('./wordlists/english.json');
    wordlists.english = _default;
    wordlists.EN = _default;
}
catch (err) { }

},{"./wordlists/chinese_simplified.json":undefined,"./wordlists/chinese_traditional.json":undefined,"./wordlists/english.json":9,"./wordlists/french.json":undefined,"./wordlists/italian.json":undefined,"./wordlists/japanese.json":undefined,"./wordlists/korean.json":undefined,"./wordlists/spanish.json":undefined}],8:[function(require,module,exports){
(function (Buffer){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const createHash = require("create-hash");
const pbkdf2_1 = require("pbkdf2");
const randomBytes = require("randombytes");
const _wordlists_1 = require("./_wordlists");
let DEFAULT_WORDLIST = _wordlists_1._default;
const INVALID_MNEMONIC = 'Invalid mnemonic';
const INVALID_ENTROPY = 'Invalid entropy';
const INVALID_CHECKSUM = 'Invalid mnemonic checksum';
const WORDLIST_REQUIRED = 'A wordlist is required but a default could not be found.\n' +
    'Please explicitly pass a 2048 word array explicitly.';
function lpad(str, padString, length) {
    while (str.length < length)
        str = padString + str;
    return str;
}
function binaryToByte(bin) {
    return parseInt(bin, 2);
}
function bytesToBinary(bytes) {
    return bytes.map(x => lpad(x.toString(2), '0', 8)).join('');
}
function deriveChecksumBits(entropyBuffer) {
    const ENT = entropyBuffer.length * 8;
    const CS = ENT / 32;
    const hash = createHash('sha256')
        .update(entropyBuffer)
        .digest();
    return bytesToBinary([...hash]).slice(0, CS);
}
function salt(password) {
    return 'mnemonic' + (password || '');
}
function mnemonicToSeedSync(mnemonic, password) {
    const mnemonicBuffer = Buffer.from((mnemonic || '').normalize('NFKD'), 'utf8');
    const saltBuffer = Buffer.from(salt((password || '').normalize('NFKD')), 'utf8');
    return pbkdf2_1.pbkdf2Sync(mnemonicBuffer, saltBuffer, 2048, 64, 'sha512');
}
exports.mnemonicToSeedSync = mnemonicToSeedSync;
function mnemonicToSeed(mnemonic, password) {
    return new Promise((resolve, reject) => {
        try {
            const mnemonicBuffer = Buffer.from((mnemonic || '').normalize('NFKD'), 'utf8');
            const saltBuffer = Buffer.from(salt((password || '').normalize('NFKD')), 'utf8');
            pbkdf2_1.pbkdf2(mnemonicBuffer, saltBuffer, 2048, 64, 'sha512', (err, data) => {
                if (err)
                    return reject(err);
                else
                    return resolve(data);
            });
        }
        catch (error) {
            return reject(error);
        }
    });
}
exports.mnemonicToSeed = mnemonicToSeed;
function mnemonicToEntropy(mnemonic, wordlist) {
    wordlist = wordlist || DEFAULT_WORDLIST;
    if (!wordlist) {
        throw new Error(WORDLIST_REQUIRED);
    }
    const words = (mnemonic || '').normalize('NFKD').split(' ');
    if (words.length % 3 !== 0)
        throw new Error(INVALID_MNEMONIC);
    // convert word indices to 11 bit binary strings
    const bits = words
        .map(word => {
        const index = wordlist.indexOf(word);
        if (index === -1)
            throw new Error(INVALID_MNEMONIC);
        return lpad(index.toString(2), '0', 11);
    })
        .join('');
    // split the binary string into ENT/CS
    const dividerIndex = Math.floor(bits.length / 33) * 32;
    const entropyBits = bits.slice(0, dividerIndex);
    const checksumBits = bits.slice(dividerIndex);
    // calculate the checksum and compare
    const entropyBytes = entropyBits.match(/(.{1,8})/g).map(binaryToByte);
    if (entropyBytes.length < 16)
        throw new Error(INVALID_ENTROPY);
    if (entropyBytes.length > 32)
        throw new Error(INVALID_ENTROPY);
    if (entropyBytes.length % 4 !== 0)
        throw new Error(INVALID_ENTROPY);
    const entropy = Buffer.from(entropyBytes);
    const newChecksum = deriveChecksumBits(entropy);
    if (newChecksum !== checksumBits)
        throw new Error(INVALID_CHECKSUM);
    return entropy.toString('hex');
}
exports.mnemonicToEntropy = mnemonicToEntropy;
function entropyToMnemonic(entropy, wordlist) {
    if (!Buffer.isBuffer(entropy))
        entropy = Buffer.from(entropy, 'hex');
    wordlist = wordlist || DEFAULT_WORDLIST;
    if (!wordlist) {
        throw new Error(WORDLIST_REQUIRED);
    }
    // 128 <= ENT <= 256
    if (entropy.length < 16)
        throw new TypeError(INVALID_ENTROPY);
    if (entropy.length > 32)
        throw new TypeError(INVALID_ENTROPY);
    if (entropy.length % 4 !== 0)
        throw new TypeError(INVALID_ENTROPY);
    const entropyBits = bytesToBinary([...entropy]);
    const checksumBits = deriveChecksumBits(entropy);
    const bits = entropyBits + checksumBits;
    const chunks = bits.match(/(.{1,11})/g);
    const words = chunks.map(binary => {
        const index = binaryToByte(binary);
        return wordlist[index];
    });
    return wordlist[0] === '\u3042\u3044\u3053\u304f\u3057\u3093' // Japanese wordlist
        ? words.join('\u3000')
        : words.join(' ');
}
exports.entropyToMnemonic = entropyToMnemonic;
function generateMnemonic(strength, rng, wordlist) {
    strength = strength || 128;
    if (strength % 32 !== 0)
        throw new TypeError(INVALID_ENTROPY);
    rng = rng || randomBytes;
    return entropyToMnemonic(rng(strength / 8), wordlist);
}
exports.generateMnemonic = generateMnemonic;
function validateMnemonic(mnemonic, wordlist) {
    try {
        mnemonicToEntropy(mnemonic, wordlist);
    }
    catch (e) {
        return false;
    }
    return true;
}
exports.validateMnemonic = validateMnemonic;
function setDefaultWordlist(language) {
    const result = _wordlists_1.wordlists[language];
    if (result)
        DEFAULT_WORDLIST = result;
    else
        throw new Error('Could not find wordlist for language "' + language + '"');
}
exports.setDefaultWordlist = setDefaultWordlist;
function getDefaultWordlist() {
    if (!DEFAULT_WORDLIST)
        throw new Error('No Default Wordlist set');
    return Object.keys(_wordlists_1.wordlists).filter(lang => {
        if (lang === 'JA' || lang === 'EN')
            return false;
        return _wordlists_1.wordlists[lang].every((word, index) => word === DEFAULT_WORDLIST[index]);
    })[0];
}
exports.getDefaultWordlist = getDefaultWordlist;
var _wordlists_2 = require("./_wordlists");
exports.wordlists = _wordlists_2.wordlists;

}).call(this,require("buffer").Buffer)
},{"./_wordlists":7,"buffer":37,"create-hash":11,"pbkdf2":16,"randombytes":21}],9:[function(require,module,exports){
module.exports=[
    "abandon",
    "ability",
    "able",
    "about",
    "above",
    "absent",
    "absorb",
    "abstract",
    "absurd",
    "abuse",
    "access",
    "accident",
    "account",
    "accuse",
    "achieve",
    "acid",
    "acoustic",
    "acquire",
    "across",
    "act",
    "action",
    "actor",
    "actress",
    "actual",
    "adapt",
    "add",
    "addict",
    "address",
    "adjust",
    "admit",
    "adult",
    "advance",
    "advice",
    "aerobic",
    "affair",
    "afford",
    "afraid",
    "again",
    "age",
    "agent",
    "agree",
    "ahead",
    "aim",
    "air",
    "airport",
    "aisle",
    "alarm",
    "album",
    "alcohol",
    "alert",
    "alien",
    "all",
    "alley",
    "allow",
    "almost",
    "alone",
    "alpha",
    "already",
    "also",
    "alter",
    "always",
    "amateur",
    "amazing",
    "among",
    "amount",
    "amused",
    "analyst",
    "anchor",
    "ancient",
    "anger",
    "angle",
    "angry",
    "animal",
    "ankle",
    "announce",
    "annual",
    "another",
    "answer",
    "antenna",
    "antique",
    "anxiety",
    "any",
    "apart",
    "apology",
    "appear",
    "apple",
    "approve",
    "april",
    "arch",
    "arctic",
    "area",
    "arena",
    "argue",
    "arm",
    "armed",
    "armor",
    "army",
    "around",
    "arrange",
    "arrest",
    "arrive",
    "arrow",
    "art",
    "artefact",
    "artist",
    "artwork",
    "ask",
    "aspect",
    "assault",
    "asset",
    "assist",
    "assume",
    "asthma",
    "athlete",
    "atom",
    "attack",
    "attend",
    "attitude",
    "attract",
    "auction",
    "audit",
    "august",
    "aunt",
    "author",
    "auto",
    "autumn",
    "average",
    "avocado",
    "avoid",
    "awake",
    "aware",
    "away",
    "awesome",
    "awful",
    "awkward",
    "axis",
    "baby",
    "bachelor",
    "bacon",
    "badge",
    "bag",
    "balance",
    "balcony",
    "ball",
    "bamboo",
    "banana",
    "banner",
    "bar",
    "barely",
    "bargain",
    "barrel",
    "base",
    "basic",
    "basket",
    "battle",
    "beach",
    "bean",
    "beauty",
    "because",
    "become",
    "beef",
    "before",
    "begin",
    "behave",
    "behind",
    "believe",
    "below",
    "belt",
    "bench",
    "benefit",
    "best",
    "betray",
    "better",
    "between",
    "beyond",
    "bicycle",
    "bid",
    "bike",
    "bind",
    "biology",
    "bird",
    "birth",
    "bitter",
    "black",
    "blade",
    "blame",
    "blanket",
    "blast",
    "bleak",
    "bless",
    "blind",
    "blood",
    "blossom",
    "blouse",
    "blue",
    "blur",
    "blush",
    "board",
    "boat",
    "body",
    "boil",
    "bomb",
    "bone",
    "bonus",
    "book",
    "boost",
    "border",
    "boring",
    "borrow",
    "boss",
    "bottom",
    "bounce",
    "box",
    "boy",
    "bracket",
    "brain",
    "brand",
    "brass",
    "brave",
    "bread",
    "breeze",
    "brick",
    "bridge",
    "brief",
    "bright",
    "bring",
    "brisk",
    "broccoli",
    "broken",
    "bronze",
    "broom",
    "brother",
    "brown",
    "brush",
    "bubble",
    "buddy",
    "budget",
    "buffalo",
    "build",
    "bulb",
    "bulk",
    "bullet",
    "bundle",
    "bunker",
    "burden",
    "burger",
    "burst",
    "bus",
    "business",
    "busy",
    "butter",
    "buyer",
    "buzz",
    "cabbage",
    "cabin",
    "cable",
    "cactus",
    "cage",
    "cake",
    "call",
    "calm",
    "camera",
    "camp",
    "can",
    "canal",
    "cancel",
    "candy",
    "cannon",
    "canoe",
    "canvas",
    "canyon",
    "capable",
    "capital",
    "captain",
    "car",
    "carbon",
    "card",
    "cargo",
    "carpet",
    "carry",
    "cart",
    "case",
    "cash",
    "casino",
    "castle",
    "casual",
    "cat",
    "catalog",
    "catch",
    "category",
    "cattle",
    "caught",
    "cause",
    "caution",
    "cave",
    "ceiling",
    "celery",
    "cement",
    "census",
    "century",
    "cereal",
    "certain",
    "chair",
    "chalk",
    "champion",
    "change",
    "chaos",
    "chapter",
    "charge",
    "chase",
    "chat",
    "cheap",
    "check",
    "cheese",
    "chef",
    "cherry",
    "chest",
    "chicken",
    "chief",
    "child",
    "chimney",
    "choice",
    "choose",
    "chronic",
    "chuckle",
    "chunk",
    "churn",
    "cigar",
    "cinnamon",
    "circle",
    "citizen",
    "city",
    "civil",
    "claim",
    "clap",
    "clarify",
    "claw",
    "clay",
    "clean",
    "clerk",
    "clever",
    "click",
    "client",
    "cliff",
    "climb",
    "clinic",
    "clip",
    "clock",
    "clog",
    "close",
    "cloth",
    "cloud",
    "clown",
    "club",
    "clump",
    "cluster",
    "clutch",
    "coach",
    "coast",
    "coconut",
    "code",
    "coffee",
    "coil",
    "coin",
    "collect",
    "color",
    "column",
    "combine",
    "come",
    "comfort",
    "comic",
    "common",
    "company",
    "concert",
    "conduct",
    "confirm",
    "congress",
    "connect",
    "consider",
    "control",
    "convince",
    "cook",
    "cool",
    "copper",
    "copy",
    "coral",
    "core",
    "corn",
    "correct",
    "cost",
    "cotton",
    "couch",
    "country",
    "couple",
    "course",
    "cousin",
    "cover",
    "coyote",
    "crack",
    "cradle",
    "craft",
    "cram",
    "crane",
    "crash",
    "crater",
    "crawl",
    "crazy",
    "cream",
    "credit",
    "creek",
    "crew",
    "cricket",
    "crime",
    "crisp",
    "critic",
    "crop",
    "cross",
    "crouch",
    "crowd",
    "crucial",
    "cruel",
    "cruise",
    "crumble",
    "crunch",
    "crush",
    "cry",
    "crystal",
    "cube",
    "culture",
    "cup",
    "cupboard",
    "curious",
    "current",
    "curtain",
    "curve",
    "cushion",
    "custom",
    "cute",
    "cycle",
    "dad",
    "damage",
    "damp",
    "dance",
    "danger",
    "daring",
    "dash",
    "daughter",
    "dawn",
    "day",
    "deal",
    "debate",
    "debris",
    "decade",
    "december",
    "decide",
    "decline",
    "decorate",
    "decrease",
    "deer",
    "defense",
    "define",
    "defy",
    "degree",
    "delay",
    "deliver",
    "demand",
    "demise",
    "denial",
    "dentist",
    "deny",
    "depart",
    "depend",
    "deposit",
    "depth",
    "deputy",
    "derive",
    "describe",
    "desert",
    "design",
    "desk",
    "despair",
    "destroy",
    "detail",
    "detect",
    "develop",
    "device",
    "devote",
    "diagram",
    "dial",
    "diamond",
    "diary",
    "dice",
    "diesel",
    "diet",
    "differ",
    "digital",
    "dignity",
    "dilemma",
    "dinner",
    "dinosaur",
    "direct",
    "dirt",
    "disagree",
    "discover",
    "disease",
    "dish",
    "dismiss",
    "disorder",
    "display",
    "distance",
    "divert",
    "divide",
    "divorce",
    "dizzy",
    "doctor",
    "document",
    "dog",
    "doll",
    "dolphin",
    "domain",
    "donate",
    "donkey",
    "donor",
    "door",
    "dose",
    "double",
    "dove",
    "draft",
    "dragon",
    "drama",
    "drastic",
    "draw",
    "dream",
    "dress",
    "drift",
    "drill",
    "drink",
    "drip",
    "drive",
    "drop",
    "drum",
    "dry",
    "duck",
    "dumb",
    "dune",
    "during",
    "dust",
    "dutch",
    "duty",
    "dwarf",
    "dynamic",
    "eager",
    "eagle",
    "early",
    "earn",
    "earth",
    "easily",
    "east",
    "easy",
    "echo",
    "ecology",
    "economy",
    "edge",
    "edit",
    "educate",
    "effort",
    "egg",
    "eight",
    "either",
    "elbow",
    "elder",
    "electric",
    "elegant",
    "element",
    "elephant",
    "elevator",
    "elite",
    "else",
    "embark",
    "embody",
    "embrace",
    "emerge",
    "emotion",
    "employ",
    "empower",
    "empty",
    "enable",
    "enact",
    "end",
    "endless",
    "endorse",
    "enemy",
    "energy",
    "enforce",
    "engage",
    "engine",
    "enhance",
    "enjoy",
    "enlist",
    "enough",
    "enrich",
    "enroll",
    "ensure",
    "enter",
    "entire",
    "entry",
    "envelope",
    "episode",
    "equal",
    "equip",
    "era",
    "erase",
    "erode",
    "erosion",
    "error",
    "erupt",
    "escape",
    "essay",
    "essence",
    "estate",
    "eternal",
    "ethics",
    "evidence",
    "evil",
    "evoke",
    "evolve",
    "exact",
    "example",
    "excess",
    "exchange",
    "excite",
    "exclude",
    "excuse",
    "execute",
    "exercise",
    "exhaust",
    "exhibit",
    "exile",
    "exist",
    "exit",
    "exotic",
    "expand",
    "expect",
    "expire",
    "explain",
    "expose",
    "express",
    "extend",
    "extra",
    "eye",
    "eyebrow",
    "fabric",
    "face",
    "faculty",
    "fade",
    "faint",
    "faith",
    "fall",
    "false",
    "fame",
    "family",
    "famous",
    "fan",
    "fancy",
    "fantasy",
    "farm",
    "fashion",
    "fat",
    "fatal",
    "father",
    "fatigue",
    "fault",
    "favorite",
    "feature",
    "february",
    "federal",
    "fee",
    "feed",
    "feel",
    "female",
    "fence",
    "festival",
    "fetch",
    "fever",
    "few",
    "fiber",
    "fiction",
    "field",
    "figure",
    "file",
    "film",
    "filter",
    "final",
    "find",
    "fine",
    "finger",
    "finish",
    "fire",
    "firm",
    "first",
    "fiscal",
    "fish",
    "fit",
    "fitness",
    "fix",
    "flag",
    "flame",
    "flash",
    "flat",
    "flavor",
    "flee",
    "flight",
    "flip",
    "float",
    "flock",
    "floor",
    "flower",
    "fluid",
    "flush",
    "fly",
    "foam",
    "focus",
    "fog",
    "foil",
    "fold",
    "follow",
    "food",
    "foot",
    "force",
    "forest",
    "forget",
    "fork",
    "fortune",
    "forum",
    "forward",
    "fossil",
    "foster",
    "found",
    "fox",
    "fragile",
    "frame",
    "frequent",
    "fresh",
    "friend",
    "fringe",
    "frog",
    "front",
    "frost",
    "frown",
    "frozen",
    "fruit",
    "fuel",
    "fun",
    "funny",
    "furnace",
    "fury",
    "future",
    "gadget",
    "gain",
    "galaxy",
    "gallery",
    "game",
    "gap",
    "garage",
    "garbage",
    "garden",
    "garlic",
    "garment",
    "gas",
    "gasp",
    "gate",
    "gather",
    "gauge",
    "gaze",
    "general",
    "genius",
    "genre",
    "gentle",
    "genuine",
    "gesture",
    "ghost",
    "giant",
    "gift",
    "giggle",
    "ginger",
    "giraffe",
    "girl",
    "give",
    "glad",
    "glance",
    "glare",
    "glass",
    "glide",
    "glimpse",
    "globe",
    "gloom",
    "glory",
    "glove",
    "glow",
    "glue",
    "goat",
    "goddess",
    "gold",
    "good",
    "goose",
    "gorilla",
    "gospel",
    "gossip",
    "govern",
    "gown",
    "grab",
    "grace",
    "grain",
    "grant",
    "grape",
    "grass",
    "gravity",
    "great",
    "green",
    "grid",
    "grief",
    "grit",
    "grocery",
    "group",
    "grow",
    "grunt",
    "guard",
    "guess",
    "guide",
    "guilt",
    "guitar",
    "gun",
    "gym",
    "habit",
    "hair",
    "half",
    "hammer",
    "hamster",
    "hand",
    "happy",
    "harbor",
    "hard",
    "harsh",
    "harvest",
    "hat",
    "have",
    "hawk",
    "hazard",
    "head",
    "health",
    "heart",
    "heavy",
    "hedgehog",
    "height",
    "hello",
    "helmet",
    "help",
    "hen",
    "hero",
    "hidden",
    "high",
    "hill",
    "hint",
    "hip",
    "hire",
    "history",
    "hobby",
    "hockey",
    "hold",
    "hole",
    "holiday",
    "hollow",
    "home",
    "honey",
    "hood",
    "hope",
    "horn",
    "horror",
    "horse",
    "hospital",
    "host",
    "hotel",
    "hour",
    "hover",
    "hub",
    "huge",
    "human",
    "humble",
    "humor",
    "hundred",
    "hungry",
    "hunt",
    "hurdle",
    "hurry",
    "hurt",
    "husband",
    "hybrid",
    "ice",
    "icon",
    "idea",
    "identify",
    "idle",
    "ignore",
    "ill",
    "illegal",
    "illness",
    "image",
    "imitate",
    "immense",
    "immune",
    "impact",
    "impose",
    "improve",
    "impulse",
    "inch",
    "include",
    "income",
    "increase",
    "index",
    "indicate",
    "indoor",
    "industry",
    "infant",
    "inflict",
    "inform",
    "inhale",
    "inherit",
    "initial",
    "inject",
    "injury",
    "inmate",
    "inner",
    "innocent",
    "input",
    "inquiry",
    "insane",
    "insect",
    "inside",
    "inspire",
    "install",
    "intact",
    "interest",
    "into",
    "invest",
    "invite",
    "involve",
    "iron",
    "island",
    "isolate",
    "issue",
    "item",
    "ivory",
    "jacket",
    "jaguar",
    "jar",
    "jazz",
    "jealous",
    "jeans",
    "jelly",
    "jewel",
    "job",
    "join",
    "joke",
    "journey",
    "joy",
    "judge",
    "juice",
    "jump",
    "jungle",
    "junior",
    "junk",
    "just",
    "kangaroo",
    "keen",
    "keep",
    "ketchup",
    "key",
    "kick",
    "kid",
    "kidney",
    "kind",
    "kingdom",
    "kiss",
    "kit",
    "kitchen",
    "kite",
    "kitten",
    "kiwi",
    "knee",
    "knife",
    "knock",
    "know",
    "lab",
    "label",
    "labor",
    "ladder",
    "lady",
    "lake",
    "lamp",
    "language",
    "laptop",
    "large",
    "later",
    "latin",
    "laugh",
    "laundry",
    "lava",
    "law",
    "lawn",
    "lawsuit",
    "layer",
    "lazy",
    "leader",
    "leaf",
    "learn",
    "leave",
    "lecture",
    "left",
    "leg",
    "legal",
    "legend",
    "leisure",
    "lemon",
    "lend",
    "length",
    "lens",
    "leopard",
    "lesson",
    "letter",
    "level",
    "liar",
    "liberty",
    "library",
    "license",
    "life",
    "lift",
    "light",
    "like",
    "limb",
    "limit",
    "link",
    "lion",
    "liquid",
    "list",
    "little",
    "live",
    "lizard",
    "load",
    "loan",
    "lobster",
    "local",
    "lock",
    "logic",
    "lonely",
    "long",
    "loop",
    "lottery",
    "loud",
    "lounge",
    "love",
    "loyal",
    "lucky",
    "luggage",
    "lumber",
    "lunar",
    "lunch",
    "luxury",
    "lyrics",
    "machine",
    "mad",
    "magic",
    "magnet",
    "maid",
    "mail",
    "main",
    "major",
    "make",
    "mammal",
    "man",
    "manage",
    "mandate",
    "mango",
    "mansion",
    "manual",
    "maple",
    "marble",
    "march",
    "margin",
    "marine",
    "market",
    "marriage",
    "mask",
    "mass",
    "master",
    "match",
    "material",
    "math",
    "matrix",
    "matter",
    "maximum",
    "maze",
    "meadow",
    "mean",
    "measure",
    "meat",
    "mechanic",
    "medal",
    "media",
    "melody",
    "melt",
    "member",
    "memory",
    "mention",
    "menu",
    "mercy",
    "merge",
    "merit",
    "merry",
    "mesh",
    "message",
    "metal",
    "method",
    "middle",
    "midnight",
    "milk",
    "million",
    "mimic",
    "mind",
    "minimum",
    "minor",
    "minute",
    "miracle",
    "mirror",
    "misery",
    "miss",
    "mistake",
    "mix",
    "mixed",
    "mixture",
    "mobile",
    "model",
    "modify",
    "mom",
    "moment",
    "monitor",
    "monkey",
    "monster",
    "month",
    "moon",
    "moral",
    "more",
    "morning",
    "mosquito",
    "mother",
    "motion",
    "motor",
    "mountain",
    "mouse",
    "move",
    "movie",
    "much",
    "muffin",
    "mule",
    "multiply",
    "muscle",
    "museum",
    "mushroom",
    "music",
    "must",
    "mutual",
    "myself",
    "mystery",
    "myth",
    "naive",
    "name",
    "napkin",
    "narrow",
    "nasty",
    "nation",
    "nature",
    "near",
    "neck",
    "need",
    "negative",
    "neglect",
    "neither",
    "nephew",
    "nerve",
    "nest",
    "net",
    "network",
    "neutral",
    "never",
    "news",
    "next",
    "nice",
    "night",
    "noble",
    "noise",
    "nominee",
    "noodle",
    "normal",
    "north",
    "nose",
    "notable",
    "note",
    "nothing",
    "notice",
    "novel",
    "now",
    "nuclear",
    "number",
    "nurse",
    "nut",
    "oak",
    "obey",
    "object",
    "oblige",
    "obscure",
    "observe",
    "obtain",
    "obvious",
    "occur",
    "ocean",
    "october",
    "odor",
    "off",
    "offer",
    "office",
    "often",
    "oil",
    "okay",
    "old",
    "olive",
    "olympic",
    "omit",
    "once",
    "one",
    "onion",
    "online",
    "only",
    "open",
    "opera",
    "opinion",
    "oppose",
    "option",
    "orange",
    "orbit",
    "orchard",
    "order",
    "ordinary",
    "organ",
    "orient",
    "original",
    "orphan",
    "ostrich",
    "other",
    "outdoor",
    "outer",
    "output",
    "outside",
    "oval",
    "oven",
    "over",
    "own",
    "owner",
    "oxygen",
    "oyster",
    "ozone",
    "pact",
    "paddle",
    "page",
    "pair",
    "palace",
    "palm",
    "panda",
    "panel",
    "panic",
    "panther",
    "paper",
    "parade",
    "parent",
    "park",
    "parrot",
    "party",
    "pass",
    "patch",
    "path",
    "patient",
    "patrol",
    "pattern",
    "pause",
    "pave",
    "payment",
    "peace",
    "peanut",
    "pear",
    "peasant",
    "pelican",
    "pen",
    "penalty",
    "pencil",
    "people",
    "pepper",
    "perfect",
    "permit",
    "person",
    "pet",
    "phone",
    "photo",
    "phrase",
    "physical",
    "piano",
    "picnic",
    "picture",
    "piece",
    "pig",
    "pigeon",
    "pill",
    "pilot",
    "pink",
    "pioneer",
    "pipe",
    "pistol",
    "pitch",
    "pizza",
    "place",
    "planet",
    "plastic",
    "plate",
    "play",
    "please",
    "pledge",
    "pluck",
    "plug",
    "plunge",
    "poem",
    "poet",
    "point",
    "polar",
    "pole",
    "police",
    "pond",
    "pony",
    "pool",
    "popular",
    "portion",
    "position",
    "possible",
    "post",
    "potato",
    "pottery",
    "poverty",
    "powder",
    "power",
    "practice",
    "praise",
    "predict",
    "prefer",
    "prepare",
    "present",
    "pretty",
    "prevent",
    "price",
    "pride",
    "primary",
    "print",
    "priority",
    "prison",
    "private",
    "prize",
    "problem",
    "process",
    "produce",
    "profit",
    "program",
    "project",
    "promote",
    "proof",
    "property",
    "prosper",
    "protect",
    "proud",
    "provide",
    "public",
    "pudding",
    "pull",
    "pulp",
    "pulse",
    "pumpkin",
    "punch",
    "pupil",
    "puppy",
    "purchase",
    "purity",
    "purpose",
    "purse",
    "push",
    "put",
    "puzzle",
    "pyramid",
    "quality",
    "quantum",
    "quarter",
    "question",
    "quick",
    "quit",
    "quiz",
    "quote",
    "rabbit",
    "raccoon",
    "race",
    "rack",
    "radar",
    "radio",
    "rail",
    "rain",
    "raise",
    "rally",
    "ramp",
    "ranch",
    "random",
    "range",
    "rapid",
    "rare",
    "rate",
    "rather",
    "raven",
    "raw",
    "razor",
    "ready",
    "real",
    "reason",
    "rebel",
    "rebuild",
    "recall",
    "receive",
    "recipe",
    "record",
    "recycle",
    "reduce",
    "reflect",
    "reform",
    "refuse",
    "region",
    "regret",
    "regular",
    "reject",
    "relax",
    "release",
    "relief",
    "rely",
    "remain",
    "remember",
    "remind",
    "remove",
    "render",
    "renew",
    "rent",
    "reopen",
    "repair",
    "repeat",
    "replace",
    "report",
    "require",
    "rescue",
    "resemble",
    "resist",
    "resource",
    "response",
    "result",
    "retire",
    "retreat",
    "return",
    "reunion",
    "reveal",
    "review",
    "reward",
    "rhythm",
    "rib",
    "ribbon",
    "rice",
    "rich",
    "ride",
    "ridge",
    "rifle",
    "right",
    "rigid",
    "ring",
    "riot",
    "ripple",
    "risk",
    "ritual",
    "rival",
    "river",
    "road",
    "roast",
    "robot",
    "robust",
    "rocket",
    "romance",
    "roof",
    "rookie",
    "room",
    "rose",
    "rotate",
    "rough",
    "round",
    "route",
    "royal",
    "rubber",
    "rude",
    "rug",
    "rule",
    "run",
    "runway",
    "rural",
    "sad",
    "saddle",
    "sadness",
    "safe",
    "sail",
    "salad",
    "salmon",
    "salon",
    "salt",
    "salute",
    "same",
    "sample",
    "sand",
    "satisfy",
    "satoshi",
    "sauce",
    "sausage",
    "save",
    "say",
    "scale",
    "scan",
    "scare",
    "scatter",
    "scene",
    "scheme",
    "school",
    "science",
    "scissors",
    "scorpion",
    "scout",
    "scrap",
    "screen",
    "script",
    "scrub",
    "sea",
    "search",
    "season",
    "seat",
    "second",
    "secret",
    "section",
    "security",
    "seed",
    "seek",
    "segment",
    "select",
    "sell",
    "seminar",
    "senior",
    "sense",
    "sentence",
    "series",
    "service",
    "session",
    "settle",
    "setup",
    "seven",
    "shadow",
    "shaft",
    "shallow",
    "share",
    "shed",
    "shell",
    "sheriff",
    "shield",
    "shift",
    "shine",
    "ship",
    "shiver",
    "shock",
    "shoe",
    "shoot",
    "shop",
    "short",
    "shoulder",
    "shove",
    "shrimp",
    "shrug",
    "shuffle",
    "shy",
    "sibling",
    "sick",
    "side",
    "siege",
    "sight",
    "sign",
    "silent",
    "silk",
    "silly",
    "silver",
    "similar",
    "simple",
    "since",
    "sing",
    "siren",
    "sister",
    "situate",
    "six",
    "size",
    "skate",
    "sketch",
    "ski",
    "skill",
    "skin",
    "skirt",
    "skull",
    "slab",
    "slam",
    "sleep",
    "slender",
    "slice",
    "slide",
    "slight",
    "slim",
    "slogan",
    "slot",
    "slow",
    "slush",
    "small",
    "smart",
    "smile",
    "smoke",
    "smooth",
    "snack",
    "snake",
    "snap",
    "sniff",
    "snow",
    "soap",
    "soccer",
    "social",
    "sock",
    "soda",
    "soft",
    "solar",
    "soldier",
    "solid",
    "solution",
    "solve",
    "someone",
    "song",
    "soon",
    "sorry",
    "sort",
    "soul",
    "sound",
    "soup",
    "source",
    "south",
    "space",
    "spare",
    "spatial",
    "spawn",
    "speak",
    "special",
    "speed",
    "spell",
    "spend",
    "sphere",
    "spice",
    "spider",
    "spike",
    "spin",
    "spirit",
    "split",
    "spoil",
    "sponsor",
    "spoon",
    "sport",
    "spot",
    "spray",
    "spread",
    "spring",
    "spy",
    "square",
    "squeeze",
    "squirrel",
    "stable",
    "stadium",
    "staff",
    "stage",
    "stairs",
    "stamp",
    "stand",
    "start",
    "state",
    "stay",
    "steak",
    "steel",
    "stem",
    "step",
    "stereo",
    "stick",
    "still",
    "sting",
    "stock",
    "stomach",
    "stone",
    "stool",
    "story",
    "stove",
    "strategy",
    "street",
    "strike",
    "strong",
    "struggle",
    "student",
    "stuff",
    "stumble",
    "style",
    "subject",
    "submit",
    "subway",
    "success",
    "such",
    "sudden",
    "suffer",
    "sugar",
    "suggest",
    "suit",
    "summer",
    "sun",
    "sunny",
    "sunset",
    "super",
    "supply",
    "supreme",
    "sure",
    "surface",
    "surge",
    "surprise",
    "surround",
    "survey",
    "suspect",
    "sustain",
    "swallow",
    "swamp",
    "swap",
    "swarm",
    "swear",
    "sweet",
    "swift",
    "swim",
    "swing",
    "switch",
    "sword",
    "symbol",
    "symptom",
    "syrup",
    "system",
    "table",
    "tackle",
    "tag",
    "tail",
    "talent",
    "talk",
    "tank",
    "tape",
    "target",
    "task",
    "taste",
    "tattoo",
    "taxi",
    "teach",
    "team",
    "tell",
    "ten",
    "tenant",
    "tennis",
    "tent",
    "term",
    "test",
    "text",
    "thank",
    "that",
    "theme",
    "then",
    "theory",
    "there",
    "they",
    "thing",
    "this",
    "thought",
    "three",
    "thrive",
    "throw",
    "thumb",
    "thunder",
    "ticket",
    "tide",
    "tiger",
    "tilt",
    "timber",
    "time",
    "tiny",
    "tip",
    "tired",
    "tissue",
    "title",
    "toast",
    "tobacco",
    "today",
    "toddler",
    "toe",
    "together",
    "toilet",
    "token",
    "tomato",
    "tomorrow",
    "tone",
    "tongue",
    "tonight",
    "tool",
    "tooth",
    "top",
    "topic",
    "topple",
    "torch",
    "tornado",
    "tortoise",
    "toss",
    "total",
    "tourist",
    "toward",
    "tower",
    "town",
    "toy",
    "track",
    "trade",
    "traffic",
    "tragic",
    "train",
    "transfer",
    "trap",
    "trash",
    "travel",
    "tray",
    "treat",
    "tree",
    "trend",
    "trial",
    "tribe",
    "trick",
    "trigger",
    "trim",
    "trip",
    "trophy",
    "trouble",
    "truck",
    "true",
    "truly",
    "trumpet",
    "trust",
    "truth",
    "try",
    "tube",
    "tuition",
    "tumble",
    "tuna",
    "tunnel",
    "turkey",
    "turn",
    "turtle",
    "twelve",
    "twenty",
    "twice",
    "twin",
    "twist",
    "two",
    "type",
    "typical",
    "ugly",
    "umbrella",
    "unable",
    "unaware",
    "uncle",
    "uncover",
    "under",
    "undo",
    "unfair",
    "unfold",
    "unhappy",
    "uniform",
    "unique",
    "unit",
    "universe",
    "unknown",
    "unlock",
    "until",
    "unusual",
    "unveil",
    "update",
    "upgrade",
    "uphold",
    "upon",
    "upper",
    "upset",
    "urban",
    "urge",
    "usage",
    "use",
    "used",
    "useful",
    "useless",
    "usual",
    "utility",
    "vacant",
    "vacuum",
    "vague",
    "valid",
    "valley",
    "valve",
    "van",
    "vanish",
    "vapor",
    "various",
    "vast",
    "vault",
    "vehicle",
    "velvet",
    "vendor",
    "venture",
    "venue",
    "verb",
    "verify",
    "version",
    "very",
    "vessel",
    "veteran",
    "viable",
    "vibrant",
    "vicious",
    "victory",
    "video",
    "view",
    "village",
    "vintage",
    "violin",
    "virtual",
    "virus",
    "visa",
    "visit",
    "visual",
    "vital",
    "vivid",
    "vocal",
    "voice",
    "void",
    "volcano",
    "volume",
    "vote",
    "voyage",
    "wage",
    "wagon",
    "wait",
    "walk",
    "wall",
    "walnut",
    "want",
    "warfare",
    "warm",
    "warrior",
    "wash",
    "wasp",
    "waste",
    "water",
    "wave",
    "way",
    "wealth",
    "weapon",
    "wear",
    "weasel",
    "weather",
    "web",
    "wedding",
    "weekend",
    "weird",
    "welcome",
    "west",
    "wet",
    "whale",
    "what",
    "wheat",
    "wheel",
    "when",
    "where",
    "whip",
    "whisper",
    "wide",
    "width",
    "wife",
    "wild",
    "will",
    "win",
    "window",
    "wine",
    "wing",
    "wink",
    "winner",
    "winter",
    "wire",
    "wisdom",
    "wise",
    "wish",
    "witness",
    "wolf",
    "woman",
    "wonder",
    "wood",
    "wool",
    "word",
    "work",
    "world",
    "worry",
    "worth",
    "wrap",
    "wreck",
    "wrestle",
    "wrist",
    "write",
    "wrong",
    "yard",
    "year",
    "yellow",
    "you",
    "young",
    "youth",
    "zebra",
    "zero",
    "zone",
    "zoo"
]

},{}],10:[function(require,module,exports){
var Buffer = require('safe-buffer').Buffer
var Transform = require('stream').Transform
var StringDecoder = require('string_decoder').StringDecoder
var inherits = require('inherits')

function CipherBase (hashMode) {
  Transform.call(this)
  this.hashMode = typeof hashMode === 'string'
  if (this.hashMode) {
    this[hashMode] = this._finalOrDigest
  } else {
    this.final = this._finalOrDigest
  }
  if (this._final) {
    this.__final = this._final
    this._final = null
  }
  this._decoder = null
  this._encoding = null
}
inherits(CipherBase, Transform)

CipherBase.prototype.update = function (data, inputEnc, outputEnc) {
  if (typeof data === 'string') {
    data = Buffer.from(data, inputEnc)
  }

  var outData = this._update(data)
  if (this.hashMode) return this

  if (outputEnc) {
    outData = this._toString(outData, outputEnc)
  }

  return outData
}

CipherBase.prototype.setAutoPadding = function () {}
CipherBase.prototype.getAuthTag = function () {
  throw new Error('trying to get auth tag in unsupported state')
}

CipherBase.prototype.setAuthTag = function () {
  throw new Error('trying to set auth tag in unsupported state')
}

CipherBase.prototype.setAAD = function () {
  throw new Error('trying to set aad in unsupported state')
}

CipherBase.prototype._transform = function (data, _, next) {
  var err
  try {
    if (this.hashMode) {
      this._update(data)
    } else {
      this.push(this._update(data))
    }
  } catch (e) {
    err = e
  } finally {
    next(err)
  }
}
CipherBase.prototype._flush = function (done) {
  var err
  try {
    this.push(this.__final())
  } catch (e) {
    err = e
  }

  done(err)
}
CipherBase.prototype._finalOrDigest = function (outputEnc) {
  var outData = this.__final() || Buffer.alloc(0)
  if (outputEnc) {
    outData = this._toString(outData, outputEnc, true)
  }
  return outData
}

CipherBase.prototype._toString = function (value, enc, fin) {
  if (!this._decoder) {
    this._decoder = new StringDecoder(enc)
    this._encoding = enc
  }

  if (this._encoding !== enc) throw new Error('can\'t switch encodings')

  var out = this._decoder.write(value)
  if (fin) {
    out += this._decoder.end()
  }

  return out
}

module.exports = CipherBase

},{"inherits":14,"safe-buffer":23,"stream":63,"string_decoder":64}],11:[function(require,module,exports){
'use strict'
var inherits = require('inherits')
var MD5 = require('md5.js')
var RIPEMD160 = require('ripemd160')
var sha = require('sha.js')
var Base = require('cipher-base')

function Hash (hash) {
  Base.call(this, 'digest')

  this._hash = hash
}

inherits(Hash, Base)

Hash.prototype._update = function (data) {
  this._hash.update(data)
}

Hash.prototype._final = function () {
  return this._hash.digest()
}

module.exports = function createHash (alg) {
  alg = alg.toLowerCase()
  if (alg === 'md5') return new MD5()
  if (alg === 'rmd160' || alg === 'ripemd160') return new RIPEMD160()

  return new Hash(sha(alg))
}

},{"cipher-base":10,"inherits":14,"md5.js":15,"ripemd160":22,"sha.js":25}],12:[function(require,module,exports){
var MD5 = require('md5.js')

module.exports = function (buffer) {
  return new MD5().update(buffer).digest()
}

},{"md5.js":15}],13:[function(require,module,exports){
'use strict'
var Buffer = require('safe-buffer').Buffer
var Transform = require('stream').Transform
var inherits = require('inherits')

function throwIfNotStringOrBuffer (val, prefix) {
  if (!Buffer.isBuffer(val) && typeof val !== 'string') {
    throw new TypeError(prefix + ' must be a string or a buffer')
  }
}

function HashBase (blockSize) {
  Transform.call(this)

  this._block = Buffer.allocUnsafe(blockSize)
  this._blockSize = blockSize
  this._blockOffset = 0
  this._length = [0, 0, 0, 0]

  this._finalized = false
}

inherits(HashBase, Transform)

HashBase.prototype._transform = function (chunk, encoding, callback) {
  var error = null
  try {
    this.update(chunk, encoding)
  } catch (err) {
    error = err
  }

  callback(error)
}

HashBase.prototype._flush = function (callback) {
  var error = null
  try {
    this.push(this.digest())
  } catch (err) {
    error = err
  }

  callback(error)
}

HashBase.prototype.update = function (data, encoding) {
  throwIfNotStringOrBuffer(data, 'Data')
  if (this._finalized) throw new Error('Digest already called')
  if (!Buffer.isBuffer(data)) data = Buffer.from(data, encoding)

  // consume data
  var block = this._block
  var offset = 0
  while (this._blockOffset + data.length - offset >= this._blockSize) {
    for (var i = this._blockOffset; i < this._blockSize;) block[i++] = data[offset++]
    this._update()
    this._blockOffset = 0
  }
  while (offset < data.length) block[this._blockOffset++] = data[offset++]

  // update length
  for (var j = 0, carry = data.length * 8; carry > 0; ++j) {
    this._length[j] += carry
    carry = (this._length[j] / 0x0100000000) | 0
    if (carry > 0) this._length[j] -= 0x0100000000 * carry
  }

  return this
}

HashBase.prototype._update = function () {
  throw new Error('_update is not implemented')
}

HashBase.prototype.digest = function (encoding) {
  if (this._finalized) throw new Error('Digest already called')
  this._finalized = true

  var digest = this._digest()
  if (encoding !== undefined) digest = digest.toString(encoding)

  // reset state
  this._block.fill(0)
  this._blockOffset = 0
  for (var i = 0; i < 4; ++i) this._length[i] = 0

  return digest
}

HashBase.prototype._digest = function () {
  throw new Error('_digest is not implemented')
}

module.exports = HashBase

},{"inherits":14,"safe-buffer":23,"stream":63}],14:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],15:[function(require,module,exports){
(function (Buffer){
'use strict'
var inherits = require('inherits')
var HashBase = require('hash-base')

var ARRAY16 = new Array(16)

function MD5 () {
  HashBase.call(this, 64)

  // state
  this._a = 0x67452301
  this._b = 0xefcdab89
  this._c = 0x98badcfe
  this._d = 0x10325476
}

inherits(MD5, HashBase)

MD5.prototype._update = function () {
  var M = ARRAY16
  for (var i = 0; i < 16; ++i) M[i] = this._block.readInt32LE(i * 4)

  var a = this._a
  var b = this._b
  var c = this._c
  var d = this._d

  a = fnF(a, b, c, d, M[0], 0xd76aa478, 7)
  d = fnF(d, a, b, c, M[1], 0xe8c7b756, 12)
  c = fnF(c, d, a, b, M[2], 0x242070db, 17)
  b = fnF(b, c, d, a, M[3], 0xc1bdceee, 22)
  a = fnF(a, b, c, d, M[4], 0xf57c0faf, 7)
  d = fnF(d, a, b, c, M[5], 0x4787c62a, 12)
  c = fnF(c, d, a, b, M[6], 0xa8304613, 17)
  b = fnF(b, c, d, a, M[7], 0xfd469501, 22)
  a = fnF(a, b, c, d, M[8], 0x698098d8, 7)
  d = fnF(d, a, b, c, M[9], 0x8b44f7af, 12)
  c = fnF(c, d, a, b, M[10], 0xffff5bb1, 17)
  b = fnF(b, c, d, a, M[11], 0x895cd7be, 22)
  a = fnF(a, b, c, d, M[12], 0x6b901122, 7)
  d = fnF(d, a, b, c, M[13], 0xfd987193, 12)
  c = fnF(c, d, a, b, M[14], 0xa679438e, 17)
  b = fnF(b, c, d, a, M[15], 0x49b40821, 22)

  a = fnG(a, b, c, d, M[1], 0xf61e2562, 5)
  d = fnG(d, a, b, c, M[6], 0xc040b340, 9)
  c = fnG(c, d, a, b, M[11], 0x265e5a51, 14)
  b = fnG(b, c, d, a, M[0], 0xe9b6c7aa, 20)
  a = fnG(a, b, c, d, M[5], 0xd62f105d, 5)
  d = fnG(d, a, b, c, M[10], 0x02441453, 9)
  c = fnG(c, d, a, b, M[15], 0xd8a1e681, 14)
  b = fnG(b, c, d, a, M[4], 0xe7d3fbc8, 20)
  a = fnG(a, b, c, d, M[9], 0x21e1cde6, 5)
  d = fnG(d, a, b, c, M[14], 0xc33707d6, 9)
  c = fnG(c, d, a, b, M[3], 0xf4d50d87, 14)
  b = fnG(b, c, d, a, M[8], 0x455a14ed, 20)
  a = fnG(a, b, c, d, M[13], 0xa9e3e905, 5)
  d = fnG(d, a, b, c, M[2], 0xfcefa3f8, 9)
  c = fnG(c, d, a, b, M[7], 0x676f02d9, 14)
  b = fnG(b, c, d, a, M[12], 0x8d2a4c8a, 20)

  a = fnH(a, b, c, d, M[5], 0xfffa3942, 4)
  d = fnH(d, a, b, c, M[8], 0x8771f681, 11)
  c = fnH(c, d, a, b, M[11], 0x6d9d6122, 16)
  b = fnH(b, c, d, a, M[14], 0xfde5380c, 23)
  a = fnH(a, b, c, d, M[1], 0xa4beea44, 4)
  d = fnH(d, a, b, c, M[4], 0x4bdecfa9, 11)
  c = fnH(c, d, a, b, M[7], 0xf6bb4b60, 16)
  b = fnH(b, c, d, a, M[10], 0xbebfbc70, 23)
  a = fnH(a, b, c, d, M[13], 0x289b7ec6, 4)
  d = fnH(d, a, b, c, M[0], 0xeaa127fa, 11)
  c = fnH(c, d, a, b, M[3], 0xd4ef3085, 16)
  b = fnH(b, c, d, a, M[6], 0x04881d05, 23)
  a = fnH(a, b, c, d, M[9], 0xd9d4d039, 4)
  d = fnH(d, a, b, c, M[12], 0xe6db99e5, 11)
  c = fnH(c, d, a, b, M[15], 0x1fa27cf8, 16)
  b = fnH(b, c, d, a, M[2], 0xc4ac5665, 23)

  a = fnI(a, b, c, d, M[0], 0xf4292244, 6)
  d = fnI(d, a, b, c, M[7], 0x432aff97, 10)
  c = fnI(c, d, a, b, M[14], 0xab9423a7, 15)
  b = fnI(b, c, d, a, M[5], 0xfc93a039, 21)
  a = fnI(a, b, c, d, M[12], 0x655b59c3, 6)
  d = fnI(d, a, b, c, M[3], 0x8f0ccc92, 10)
  c = fnI(c, d, a, b, M[10], 0xffeff47d, 15)
  b = fnI(b, c, d, a, M[1], 0x85845dd1, 21)
  a = fnI(a, b, c, d, M[8], 0x6fa87e4f, 6)
  d = fnI(d, a, b, c, M[15], 0xfe2ce6e0, 10)
  c = fnI(c, d, a, b, M[6], 0xa3014314, 15)
  b = fnI(b, c, d, a, M[13], 0x4e0811a1, 21)
  a = fnI(a, b, c, d, M[4], 0xf7537e82, 6)
  d = fnI(d, a, b, c, M[11], 0xbd3af235, 10)
  c = fnI(c, d, a, b, M[2], 0x2ad7d2bb, 15)
  b = fnI(b, c, d, a, M[9], 0xeb86d391, 21)

  this._a = (this._a + a) | 0
  this._b = (this._b + b) | 0
  this._c = (this._c + c) | 0
  this._d = (this._d + d) | 0
}

MD5.prototype._digest = function () {
  // create padding and handle blocks
  this._block[this._blockOffset++] = 0x80
  if (this._blockOffset > 56) {
    this._block.fill(0, this._blockOffset, 64)
    this._update()
    this._blockOffset = 0
  }

  this._block.fill(0, this._blockOffset, 56)
  this._block.writeUInt32LE(this._length[0], 56)
  this._block.writeUInt32LE(this._length[1], 60)
  this._update()

  // produce result
  var buffer = new Buffer(16)
  buffer.writeInt32LE(this._a, 0)
  buffer.writeInt32LE(this._b, 4)
  buffer.writeInt32LE(this._c, 8)
  buffer.writeInt32LE(this._d, 12)
  return buffer
}

function rotl (x, n) {
  return (x << n) | (x >>> (32 - n))
}

function fnF (a, b, c, d, m, k, s) {
  return (rotl((a + ((b & c) | ((~b) & d)) + m + k) | 0, s) + b) | 0
}

function fnG (a, b, c, d, m, k, s) {
  return (rotl((a + ((b & d) | (c & (~d))) + m + k) | 0, s) + b) | 0
}

function fnH (a, b, c, d, m, k, s) {
  return (rotl((a + (b ^ c ^ d) + m + k) | 0, s) + b) | 0
}

function fnI (a, b, c, d, m, k, s) {
  return (rotl((a + ((c ^ (b | (~d)))) + m + k) | 0, s) + b) | 0
}

module.exports = MD5

}).call(this,require("buffer").Buffer)
},{"buffer":37,"hash-base":13,"inherits":14}],16:[function(require,module,exports){
exports.pbkdf2 = require('./lib/async')
exports.pbkdf2Sync = require('./lib/sync')

},{"./lib/async":17,"./lib/sync":20}],17:[function(require,module,exports){
(function (process,global){
var checkParameters = require('./precondition')
var defaultEncoding = require('./default-encoding')
var sync = require('./sync')
var Buffer = require('safe-buffer').Buffer

var ZERO_BUF
var subtle = global.crypto && global.crypto.subtle
var toBrowser = {
  'sha': 'SHA-1',
  'sha-1': 'SHA-1',
  'sha1': 'SHA-1',
  'sha256': 'SHA-256',
  'sha-256': 'SHA-256',
  'sha384': 'SHA-384',
  'sha-384': 'SHA-384',
  'sha-512': 'SHA-512',
  'sha512': 'SHA-512'
}
var checks = []
function checkNative (algo) {
  if (global.process && !global.process.browser) {
    return Promise.resolve(false)
  }
  if (!subtle || !subtle.importKey || !subtle.deriveBits) {
    return Promise.resolve(false)
  }
  if (checks[algo] !== undefined) {
    return checks[algo]
  }
  ZERO_BUF = ZERO_BUF || Buffer.alloc(8)
  var prom = browserPbkdf2(ZERO_BUF, ZERO_BUF, 10, 128, algo)
    .then(function () {
      return true
    }).catch(function () {
      return false
    })
  checks[algo] = prom
  return prom
}

function browserPbkdf2 (password, salt, iterations, length, algo) {
  return subtle.importKey(
    'raw', password, {name: 'PBKDF2'}, false, ['deriveBits']
  ).then(function (key) {
    return subtle.deriveBits({
      name: 'PBKDF2',
      salt: salt,
      iterations: iterations,
      hash: {
        name: algo
      }
    }, key, length << 3)
  }).then(function (res) {
    return Buffer.from(res)
  })
}

function resolvePromise (promise, callback) {
  promise.then(function (out) {
    process.nextTick(function () {
      callback(null, out)
    })
  }, function (e) {
    process.nextTick(function () {
      callback(e)
    })
  })
}
module.exports = function (password, salt, iterations, keylen, digest, callback) {
  if (typeof digest === 'function') {
    callback = digest
    digest = undefined
  }

  digest = digest || 'sha1'
  var algo = toBrowser[digest.toLowerCase()]

  if (!algo || typeof global.Promise !== 'function') {
    return process.nextTick(function () {
      var out
      try {
        out = sync(password, salt, iterations, keylen, digest)
      } catch (e) {
        return callback(e)
      }
      callback(null, out)
    })
  }

  checkParameters(password, salt, iterations, keylen)
  if (typeof callback !== 'function') throw new Error('No callback provided to pbkdf2')
  if (!Buffer.isBuffer(password)) password = Buffer.from(password, defaultEncoding)
  if (!Buffer.isBuffer(salt)) salt = Buffer.from(salt, defaultEncoding)

  resolvePromise(checkNative(algo).then(function (resp) {
    if (resp) return browserPbkdf2(password, salt, iterations, keylen, algo)

    return sync(password, salt, iterations, keylen, digest)
  }), callback)
}

}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./default-encoding":18,"./precondition":19,"./sync":20,"_process":46,"safe-buffer":23}],18:[function(require,module,exports){
(function (process){
var defaultEncoding
/* istanbul ignore next */
if (process.browser) {
  defaultEncoding = 'utf-8'
} else {
  var pVersionMajor = parseInt(process.version.split('.')[0].slice(1), 10)

  defaultEncoding = pVersionMajor >= 6 ? 'utf-8' : 'binary'
}
module.exports = defaultEncoding

}).call(this,require('_process'))
},{"_process":46}],19:[function(require,module,exports){
(function (Buffer){
var MAX_ALLOC = Math.pow(2, 30) - 1 // default in iojs

function checkBuffer (buf, name) {
  if (typeof buf !== 'string' && !Buffer.isBuffer(buf)) {
    throw new TypeError(name + ' must be a buffer or string')
  }
}

module.exports = function (password, salt, iterations, keylen) {
  checkBuffer(password, 'Password')
  checkBuffer(salt, 'Salt')

  if (typeof iterations !== 'number') {
    throw new TypeError('Iterations not a number')
  }

  if (iterations < 0) {
    throw new TypeError('Bad iterations')
  }

  if (typeof keylen !== 'number') {
    throw new TypeError('Key length not a number')
  }

  if (keylen < 0 || keylen > MAX_ALLOC || keylen !== keylen) { /* eslint no-self-compare: 0 */
    throw new TypeError('Bad key length')
  }
}

}).call(this,{"isBuffer":require("../../../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js")})
},{"../../../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js":42}],20:[function(require,module,exports){
var md5 = require('create-hash/md5')
var RIPEMD160 = require('ripemd160')
var sha = require('sha.js')

var checkParameters = require('./precondition')
var defaultEncoding = require('./default-encoding')
var Buffer = require('safe-buffer').Buffer
var ZEROS = Buffer.alloc(128)
var sizes = {
  md5: 16,
  sha1: 20,
  sha224: 28,
  sha256: 32,
  sha384: 48,
  sha512: 64,
  rmd160: 20,
  ripemd160: 20
}

function Hmac (alg, key, saltLen) {
  var hash = getDigest(alg)
  var blocksize = (alg === 'sha512' || alg === 'sha384') ? 128 : 64

  if (key.length > blocksize) {
    key = hash(key)
  } else if (key.length < blocksize) {
    key = Buffer.concat([key, ZEROS], blocksize)
  }

  var ipad = Buffer.allocUnsafe(blocksize + sizes[alg])
  var opad = Buffer.allocUnsafe(blocksize + sizes[alg])
  for (var i = 0; i < blocksize; i++) {
    ipad[i] = key[i] ^ 0x36
    opad[i] = key[i] ^ 0x5C
  }

  var ipad1 = Buffer.allocUnsafe(blocksize + saltLen + 4)
  ipad.copy(ipad1, 0, 0, blocksize)
  this.ipad1 = ipad1
  this.ipad2 = ipad
  this.opad = opad
  this.alg = alg
  this.blocksize = blocksize
  this.hash = hash
  this.size = sizes[alg]
}

Hmac.prototype.run = function (data, ipad) {
  data.copy(ipad, this.blocksize)
  var h = this.hash(ipad)
  h.copy(this.opad, this.blocksize)
  return this.hash(this.opad)
}

function getDigest (alg) {
  function shaFunc (data) {
    return sha(alg).update(data).digest()
  }
  function rmd160Func (data) {
    return new RIPEMD160().update(data).digest()
  }

  if (alg === 'rmd160' || alg === 'ripemd160') return rmd160Func
  if (alg === 'md5') return md5
  return shaFunc
}

function pbkdf2 (password, salt, iterations, keylen, digest) {
  checkParameters(password, salt, iterations, keylen)

  if (!Buffer.isBuffer(password)) password = Buffer.from(password, defaultEncoding)
  if (!Buffer.isBuffer(salt)) salt = Buffer.from(salt, defaultEncoding)

  digest = digest || 'sha1'

  var hmac = new Hmac(digest, password, salt.length)

  var DK = Buffer.allocUnsafe(keylen)
  var block1 = Buffer.allocUnsafe(salt.length + 4)
  salt.copy(block1, 0, 0, salt.length)

  var destPos = 0
  var hLen = sizes[digest]
  var l = Math.ceil(keylen / hLen)

  for (var i = 1; i <= l; i++) {
    block1.writeUInt32BE(i, salt.length)

    var T = hmac.run(block1, hmac.ipad1)
    var U = T

    for (var j = 1; j < iterations; j++) {
      U = hmac.run(U, hmac.ipad2)
      for (var k = 0; k < hLen; k++) T[k] ^= U[k]
    }

    T.copy(DK, destPos)
    destPos += hLen
  }

  return DK
}

module.exports = pbkdf2

},{"./default-encoding":18,"./precondition":19,"create-hash/md5":12,"ripemd160":22,"safe-buffer":23,"sha.js":25}],21:[function(require,module,exports){
(function (process,global){
'use strict'

// limit of Crypto.getRandomValues()
// https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues
var MAX_BYTES = 65536

// Node supports requesting up to this number of bytes
// https://github.com/nodejs/node/blob/master/lib/internal/crypto/random.js#L48
var MAX_UINT32 = 4294967295

function oldBrowser () {
  throw new Error('Secure random number generation is not supported by this browser.\nUse Chrome, Firefox or Internet Explorer 11')
}

var Buffer = require('safe-buffer').Buffer
var crypto = global.crypto || global.msCrypto

if (crypto && crypto.getRandomValues) {
  module.exports = randomBytes
} else {
  module.exports = oldBrowser
}

function randomBytes (size, cb) {
  // phantomjs needs to throw
  if (size > MAX_UINT32) throw new RangeError('requested too many random bytes')

  var bytes = Buffer.allocUnsafe(size)

  if (size > 0) {  // getRandomValues fails on IE if size == 0
    if (size > MAX_BYTES) { // this is the max bytes crypto.getRandomValues
      // can do at once see https://developer.mozilla.org/en-US/docs/Web/API/window.crypto.getRandomValues
      for (var generated = 0; generated < size; generated += MAX_BYTES) {
        // buffer.slice automatically checks if the end is past the end of
        // the buffer so we don't have to here
        crypto.getRandomValues(bytes.slice(generated, generated + MAX_BYTES))
      }
    } else {
      crypto.getRandomValues(bytes)
    }
  }

  if (typeof cb === 'function') {
    return process.nextTick(function () {
      cb(null, bytes)
    })
  }

  return bytes
}

}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"_process":46,"safe-buffer":23}],22:[function(require,module,exports){
'use strict'
var Buffer = require('buffer').Buffer
var inherits = require('inherits')
var HashBase = require('hash-base')

var ARRAY16 = new Array(16)

var zl = [
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
  7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
  3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
  1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
  4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13
]

var zr = [
  5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
  6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
  15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
  8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
  12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11
]

var sl = [
  11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
  7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
  11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
  11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
  9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6
]

var sr = [
  8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
  9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
  9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
  15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
  8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11
]

var hl = [0x00000000, 0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xa953fd4e]
var hr = [0x50a28be6, 0x5c4dd124, 0x6d703ef3, 0x7a6d76e9, 0x00000000]

function RIPEMD160 () {
  HashBase.call(this, 64)

  // state
  this._a = 0x67452301
  this._b = 0xefcdab89
  this._c = 0x98badcfe
  this._d = 0x10325476
  this._e = 0xc3d2e1f0
}

inherits(RIPEMD160, HashBase)

RIPEMD160.prototype._update = function () {
  var words = ARRAY16
  for (var j = 0; j < 16; ++j) words[j] = this._block.readInt32LE(j * 4)

  var al = this._a | 0
  var bl = this._b | 0
  var cl = this._c | 0
  var dl = this._d | 0
  var el = this._e | 0

  var ar = this._a | 0
  var br = this._b | 0
  var cr = this._c | 0
  var dr = this._d | 0
  var er = this._e | 0

  // computation
  for (var i = 0; i < 80; i += 1) {
    var tl
    var tr
    if (i < 16) {
      tl = fn1(al, bl, cl, dl, el, words[zl[i]], hl[0], sl[i])
      tr = fn5(ar, br, cr, dr, er, words[zr[i]], hr[0], sr[i])
    } else if (i < 32) {
      tl = fn2(al, bl, cl, dl, el, words[zl[i]], hl[1], sl[i])
      tr = fn4(ar, br, cr, dr, er, words[zr[i]], hr[1], sr[i])
    } else if (i < 48) {
      tl = fn3(al, bl, cl, dl, el, words[zl[i]], hl[2], sl[i])
      tr = fn3(ar, br, cr, dr, er, words[zr[i]], hr[2], sr[i])
    } else if (i < 64) {
      tl = fn4(al, bl, cl, dl, el, words[zl[i]], hl[3], sl[i])
      tr = fn2(ar, br, cr, dr, er, words[zr[i]], hr[3], sr[i])
    } else { // if (i<80) {
      tl = fn5(al, bl, cl, dl, el, words[zl[i]], hl[4], sl[i])
      tr = fn1(ar, br, cr, dr, er, words[zr[i]], hr[4], sr[i])
    }

    al = el
    el = dl
    dl = rotl(cl, 10)
    cl = bl
    bl = tl

    ar = er
    er = dr
    dr = rotl(cr, 10)
    cr = br
    br = tr
  }

  // update state
  var t = (this._b + cl + dr) | 0
  this._b = (this._c + dl + er) | 0
  this._c = (this._d + el + ar) | 0
  this._d = (this._e + al + br) | 0
  this._e = (this._a + bl + cr) | 0
  this._a = t
}

RIPEMD160.prototype._digest = function () {
  // create padding and handle blocks
  this._block[this._blockOffset++] = 0x80
  if (this._blockOffset > 56) {
    this._block.fill(0, this._blockOffset, 64)
    this._update()
    this._blockOffset = 0
  }

  this._block.fill(0, this._blockOffset, 56)
  this._block.writeUInt32LE(this._length[0], 56)
  this._block.writeUInt32LE(this._length[1], 60)
  this._update()

  // produce result
  var buffer = Buffer.alloc ? Buffer.alloc(20) : new Buffer(20)
  buffer.writeInt32LE(this._a, 0)
  buffer.writeInt32LE(this._b, 4)
  buffer.writeInt32LE(this._c, 8)
  buffer.writeInt32LE(this._d, 12)
  buffer.writeInt32LE(this._e, 16)
  return buffer
}

function rotl (x, n) {
  return (x << n) | (x >>> (32 - n))
}

function fn1 (a, b, c, d, e, m, k, s) {
  return (rotl((a + (b ^ c ^ d) + m + k) | 0, s) + e) | 0
}

function fn2 (a, b, c, d, e, m, k, s) {
  return (rotl((a + ((b & c) | ((~b) & d)) + m + k) | 0, s) + e) | 0
}

function fn3 (a, b, c, d, e, m, k, s) {
  return (rotl((a + ((b | (~c)) ^ d) + m + k) | 0, s) + e) | 0
}

function fn4 (a, b, c, d, e, m, k, s) {
  return (rotl((a + ((b & d) | (c & (~d))) + m + k) | 0, s) + e) | 0
}

function fn5 (a, b, c, d, e, m, k, s) {
  return (rotl((a + (b ^ (c | (~d))) + m + k) | 0, s) + e) | 0
}

module.exports = RIPEMD160

},{"buffer":37,"hash-base":13,"inherits":14}],23:[function(require,module,exports){
/* eslint-disable node/no-deprecated-api */
var buffer = require('buffer')
var Buffer = buffer.Buffer

// alternative to using Object.keys for old browsers
function copyProps (src, dst) {
  for (var key in src) {
    dst[key] = src[key]
  }
}
if (Buffer.from && Buffer.alloc && Buffer.allocUnsafe && Buffer.allocUnsafeSlow) {
  module.exports = buffer
} else {
  // Copy properties from require('buffer')
  copyProps(buffer, exports)
  exports.Buffer = SafeBuffer
}

function SafeBuffer (arg, encodingOrOffset, length) {
  return Buffer(arg, encodingOrOffset, length)
}

// Copy static methods from Buffer
copyProps(Buffer, SafeBuffer)

SafeBuffer.from = function (arg, encodingOrOffset, length) {
  if (typeof arg === 'number') {
    throw new TypeError('Argument must not be a number')
  }
  return Buffer(arg, encodingOrOffset, length)
}

SafeBuffer.alloc = function (size, fill, encoding) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  var buf = Buffer(size)
  if (fill !== undefined) {
    if (typeof encoding === 'string') {
      buf.fill(fill, encoding)
    } else {
      buf.fill(fill)
    }
  } else {
    buf.fill(0)
  }
  return buf
}

SafeBuffer.allocUnsafe = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return Buffer(size)
}

SafeBuffer.allocUnsafeSlow = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return buffer.SlowBuffer(size)
}

},{"buffer":37}],24:[function(require,module,exports){
var Buffer = require('safe-buffer').Buffer

// prototype class for hash functions
function Hash (blockSize, finalSize) {
  this._block = Buffer.alloc(blockSize)
  this._finalSize = finalSize
  this._blockSize = blockSize
  this._len = 0
}

Hash.prototype.update = function (data, enc) {
  if (typeof data === 'string') {
    enc = enc || 'utf8'
    data = Buffer.from(data, enc)
  }

  var block = this._block
  var blockSize = this._blockSize
  var length = data.length
  var accum = this._len

  for (var offset = 0; offset < length;) {
    var assigned = accum % blockSize
    var remainder = Math.min(length - offset, blockSize - assigned)

    for (var i = 0; i < remainder; i++) {
      block[assigned + i] = data[offset + i]
    }

    accum += remainder
    offset += remainder

    if ((accum % blockSize) === 0) {
      this._update(block)
    }
  }

  this._len += length
  return this
}

Hash.prototype.digest = function (enc) {
  var rem = this._len % this._blockSize

  this._block[rem] = 0x80

  // zero (rem + 1) trailing bits, where (rem + 1) is the smallest
  // non-negative solution to the equation (length + 1 + (rem + 1)) === finalSize mod blockSize
  this._block.fill(0, rem + 1)

  if (rem >= this._finalSize) {
    this._update(this._block)
    this._block.fill(0)
  }

  var bits = this._len * 8

  // uint32
  if (bits <= 0xffffffff) {
    this._block.writeUInt32BE(bits, this._blockSize - 4)

  // uint64
  } else {
    var lowBits = (bits & 0xffffffff) >>> 0
    var highBits = (bits - lowBits) / 0x100000000

    this._block.writeUInt32BE(highBits, this._blockSize - 8)
    this._block.writeUInt32BE(lowBits, this._blockSize - 4)
  }

  this._update(this._block)
  var hash = this._hash()

  return enc ? hash.toString(enc) : hash
}

Hash.prototype._update = function () {
  throw new Error('_update must be implemented by subclass')
}

module.exports = Hash

},{"safe-buffer":23}],25:[function(require,module,exports){
var exports = module.exports = function SHA (algorithm) {
  algorithm = algorithm.toLowerCase()

  var Algorithm = exports[algorithm]
  if (!Algorithm) throw new Error(algorithm + ' is not supported (we accept pull requests)')

  return new Algorithm()
}

exports.sha = require('./sha')
exports.sha1 = require('./sha1')
exports.sha224 = require('./sha224')
exports.sha256 = require('./sha256')
exports.sha384 = require('./sha384')
exports.sha512 = require('./sha512')

},{"./sha":26,"./sha1":27,"./sha224":28,"./sha256":29,"./sha384":30,"./sha512":31}],26:[function(require,module,exports){
/*
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-0, as defined
 * in FIPS PUB 180-1
 * This source code is derived from sha1.js of the same repository.
 * The difference between SHA-0 and SHA-1 is just a bitwise rotate left
 * operation was added.
 */

var inherits = require('inherits')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var K = [
  0x5a827999, 0x6ed9eba1, 0x8f1bbcdc | 0, 0xca62c1d6 | 0
]

var W = new Array(80)

function Sha () {
  this.init()
  this._w = W

  Hash.call(this, 64, 56)
}

inherits(Sha, Hash)

Sha.prototype.init = function () {
  this._a = 0x67452301
  this._b = 0xefcdab89
  this._c = 0x98badcfe
  this._d = 0x10325476
  this._e = 0xc3d2e1f0

  return this
}

function rotl5 (num) {
  return (num << 5) | (num >>> 27)
}

function rotl30 (num) {
  return (num << 30) | (num >>> 2)
}

function ft (s, b, c, d) {
  if (s === 0) return (b & c) | ((~b) & d)
  if (s === 2) return (b & c) | (b & d) | (c & d)
  return b ^ c ^ d
}

Sha.prototype._update = function (M) {
  var W = this._w

  var a = this._a | 0
  var b = this._b | 0
  var c = this._c | 0
  var d = this._d | 0
  var e = this._e | 0

  for (var i = 0; i < 16; ++i) W[i] = M.readInt32BE(i * 4)
  for (; i < 80; ++i) W[i] = W[i - 3] ^ W[i - 8] ^ W[i - 14] ^ W[i - 16]

  for (var j = 0; j < 80; ++j) {
    var s = ~~(j / 20)
    var t = (rotl5(a) + ft(s, b, c, d) + e + W[j] + K[s]) | 0

    e = d
    d = c
    c = rotl30(b)
    b = a
    a = t
  }

  this._a = (a + this._a) | 0
  this._b = (b + this._b) | 0
  this._c = (c + this._c) | 0
  this._d = (d + this._d) | 0
  this._e = (e + this._e) | 0
}

Sha.prototype._hash = function () {
  var H = Buffer.allocUnsafe(20)

  H.writeInt32BE(this._a | 0, 0)
  H.writeInt32BE(this._b | 0, 4)
  H.writeInt32BE(this._c | 0, 8)
  H.writeInt32BE(this._d | 0, 12)
  H.writeInt32BE(this._e | 0, 16)

  return H
}

module.exports = Sha

},{"./hash":24,"inherits":14,"safe-buffer":23}],27:[function(require,module,exports){
/*
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-1, as defined
 * in FIPS PUB 180-1
 * Version 2.1a Copyright Paul Johnston 2000 - 2002.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for details.
 */

var inherits = require('inherits')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var K = [
  0x5a827999, 0x6ed9eba1, 0x8f1bbcdc | 0, 0xca62c1d6 | 0
]

var W = new Array(80)

function Sha1 () {
  this.init()
  this._w = W

  Hash.call(this, 64, 56)
}

inherits(Sha1, Hash)

Sha1.prototype.init = function () {
  this._a = 0x67452301
  this._b = 0xefcdab89
  this._c = 0x98badcfe
  this._d = 0x10325476
  this._e = 0xc3d2e1f0

  return this
}

function rotl1 (num) {
  return (num << 1) | (num >>> 31)
}

function rotl5 (num) {
  return (num << 5) | (num >>> 27)
}

function rotl30 (num) {
  return (num << 30) | (num >>> 2)
}

function ft (s, b, c, d) {
  if (s === 0) return (b & c) | ((~b) & d)
  if (s === 2) return (b & c) | (b & d) | (c & d)
  return b ^ c ^ d
}

Sha1.prototype._update = function (M) {
  var W = this._w

  var a = this._a | 0
  var b = this._b | 0
  var c = this._c | 0
  var d = this._d | 0
  var e = this._e | 0

  for (var i = 0; i < 16; ++i) W[i] = M.readInt32BE(i * 4)
  for (; i < 80; ++i) W[i] = rotl1(W[i - 3] ^ W[i - 8] ^ W[i - 14] ^ W[i - 16])

  for (var j = 0; j < 80; ++j) {
    var s = ~~(j / 20)
    var t = (rotl5(a) + ft(s, b, c, d) + e + W[j] + K[s]) | 0

    e = d
    d = c
    c = rotl30(b)
    b = a
    a = t
  }

  this._a = (a + this._a) | 0
  this._b = (b + this._b) | 0
  this._c = (c + this._c) | 0
  this._d = (d + this._d) | 0
  this._e = (e + this._e) | 0
}

Sha1.prototype._hash = function () {
  var H = Buffer.allocUnsafe(20)

  H.writeInt32BE(this._a | 0, 0)
  H.writeInt32BE(this._b | 0, 4)
  H.writeInt32BE(this._c | 0, 8)
  H.writeInt32BE(this._d | 0, 12)
  H.writeInt32BE(this._e | 0, 16)

  return H
}

module.exports = Sha1

},{"./hash":24,"inherits":14,"safe-buffer":23}],28:[function(require,module,exports){
/**
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-256, as defined
 * in FIPS 180-2
 * Version 2.2-beta Copyright Angel Marin, Paul Johnston 2000 - 2009.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 *
 */

var inherits = require('inherits')
var Sha256 = require('./sha256')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var W = new Array(64)

function Sha224 () {
  this.init()

  this._w = W // new Array(64)

  Hash.call(this, 64, 56)
}

inherits(Sha224, Sha256)

Sha224.prototype.init = function () {
  this._a = 0xc1059ed8
  this._b = 0x367cd507
  this._c = 0x3070dd17
  this._d = 0xf70e5939
  this._e = 0xffc00b31
  this._f = 0x68581511
  this._g = 0x64f98fa7
  this._h = 0xbefa4fa4

  return this
}

Sha224.prototype._hash = function () {
  var H = Buffer.allocUnsafe(28)

  H.writeInt32BE(this._a, 0)
  H.writeInt32BE(this._b, 4)
  H.writeInt32BE(this._c, 8)
  H.writeInt32BE(this._d, 12)
  H.writeInt32BE(this._e, 16)
  H.writeInt32BE(this._f, 20)
  H.writeInt32BE(this._g, 24)

  return H
}

module.exports = Sha224

},{"./hash":24,"./sha256":29,"inherits":14,"safe-buffer":23}],29:[function(require,module,exports){
/**
 * A JavaScript implementation of the Secure Hash Algorithm, SHA-256, as defined
 * in FIPS 180-2
 * Version 2.2-beta Copyright Angel Marin, Paul Johnston 2000 - 2009.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 *
 */

var inherits = require('inherits')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var K = [
  0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5,
  0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
  0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
  0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
  0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC,
  0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
  0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7,
  0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
  0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
  0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
  0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3,
  0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
  0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5,
  0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
  0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
  0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2
]

var W = new Array(64)

function Sha256 () {
  this.init()

  this._w = W // new Array(64)

  Hash.call(this, 64, 56)
}

inherits(Sha256, Hash)

Sha256.prototype.init = function () {
  this._a = 0x6a09e667
  this._b = 0xbb67ae85
  this._c = 0x3c6ef372
  this._d = 0xa54ff53a
  this._e = 0x510e527f
  this._f = 0x9b05688c
  this._g = 0x1f83d9ab
  this._h = 0x5be0cd19

  return this
}

function ch (x, y, z) {
  return z ^ (x & (y ^ z))
}

function maj (x, y, z) {
  return (x & y) | (z & (x | y))
}

function sigma0 (x) {
  return (x >>> 2 | x << 30) ^ (x >>> 13 | x << 19) ^ (x >>> 22 | x << 10)
}

function sigma1 (x) {
  return (x >>> 6 | x << 26) ^ (x >>> 11 | x << 21) ^ (x >>> 25 | x << 7)
}

function gamma0 (x) {
  return (x >>> 7 | x << 25) ^ (x >>> 18 | x << 14) ^ (x >>> 3)
}

function gamma1 (x) {
  return (x >>> 17 | x << 15) ^ (x >>> 19 | x << 13) ^ (x >>> 10)
}

Sha256.prototype._update = function (M) {
  var W = this._w

  var a = this._a | 0
  var b = this._b | 0
  var c = this._c | 0
  var d = this._d | 0
  var e = this._e | 0
  var f = this._f | 0
  var g = this._g | 0
  var h = this._h | 0

  for (var i = 0; i < 16; ++i) W[i] = M.readInt32BE(i * 4)
  for (; i < 64; ++i) W[i] = (gamma1(W[i - 2]) + W[i - 7] + gamma0(W[i - 15]) + W[i - 16]) | 0

  for (var j = 0; j < 64; ++j) {
    var T1 = (h + sigma1(e) + ch(e, f, g) + K[j] + W[j]) | 0
    var T2 = (sigma0(a) + maj(a, b, c)) | 0

    h = g
    g = f
    f = e
    e = (d + T1) | 0
    d = c
    c = b
    b = a
    a = (T1 + T2) | 0
  }

  this._a = (a + this._a) | 0
  this._b = (b + this._b) | 0
  this._c = (c + this._c) | 0
  this._d = (d + this._d) | 0
  this._e = (e + this._e) | 0
  this._f = (f + this._f) | 0
  this._g = (g + this._g) | 0
  this._h = (h + this._h) | 0
}

Sha256.prototype._hash = function () {
  var H = Buffer.allocUnsafe(32)

  H.writeInt32BE(this._a, 0)
  H.writeInt32BE(this._b, 4)
  H.writeInt32BE(this._c, 8)
  H.writeInt32BE(this._d, 12)
  H.writeInt32BE(this._e, 16)
  H.writeInt32BE(this._f, 20)
  H.writeInt32BE(this._g, 24)
  H.writeInt32BE(this._h, 28)

  return H
}

module.exports = Sha256

},{"./hash":24,"inherits":14,"safe-buffer":23}],30:[function(require,module,exports){
var inherits = require('inherits')
var SHA512 = require('./sha512')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var W = new Array(160)

function Sha384 () {
  this.init()
  this._w = W

  Hash.call(this, 128, 112)
}

inherits(Sha384, SHA512)

Sha384.prototype.init = function () {
  this._ah = 0xcbbb9d5d
  this._bh = 0x629a292a
  this._ch = 0x9159015a
  this._dh = 0x152fecd8
  this._eh = 0x67332667
  this._fh = 0x8eb44a87
  this._gh = 0xdb0c2e0d
  this._hh = 0x47b5481d

  this._al = 0xc1059ed8
  this._bl = 0x367cd507
  this._cl = 0x3070dd17
  this._dl = 0xf70e5939
  this._el = 0xffc00b31
  this._fl = 0x68581511
  this._gl = 0x64f98fa7
  this._hl = 0xbefa4fa4

  return this
}

Sha384.prototype._hash = function () {
  var H = Buffer.allocUnsafe(48)

  function writeInt64BE (h, l, offset) {
    H.writeInt32BE(h, offset)
    H.writeInt32BE(l, offset + 4)
  }

  writeInt64BE(this._ah, this._al, 0)
  writeInt64BE(this._bh, this._bl, 8)
  writeInt64BE(this._ch, this._cl, 16)
  writeInt64BE(this._dh, this._dl, 24)
  writeInt64BE(this._eh, this._el, 32)
  writeInt64BE(this._fh, this._fl, 40)

  return H
}

module.exports = Sha384

},{"./hash":24,"./sha512":31,"inherits":14,"safe-buffer":23}],31:[function(require,module,exports){
var inherits = require('inherits')
var Hash = require('./hash')
var Buffer = require('safe-buffer').Buffer

var K = [
  0x428a2f98, 0xd728ae22, 0x71374491, 0x23ef65cd,
  0xb5c0fbcf, 0xec4d3b2f, 0xe9b5dba5, 0x8189dbbc,
  0x3956c25b, 0xf348b538, 0x59f111f1, 0xb605d019,
  0x923f82a4, 0xaf194f9b, 0xab1c5ed5, 0xda6d8118,
  0xd807aa98, 0xa3030242, 0x12835b01, 0x45706fbe,
  0x243185be, 0x4ee4b28c, 0x550c7dc3, 0xd5ffb4e2,
  0x72be5d74, 0xf27b896f, 0x80deb1fe, 0x3b1696b1,
  0x9bdc06a7, 0x25c71235, 0xc19bf174, 0xcf692694,
  0xe49b69c1, 0x9ef14ad2, 0xefbe4786, 0x384f25e3,
  0x0fc19dc6, 0x8b8cd5b5, 0x240ca1cc, 0x77ac9c65,
  0x2de92c6f, 0x592b0275, 0x4a7484aa, 0x6ea6e483,
  0x5cb0a9dc, 0xbd41fbd4, 0x76f988da, 0x831153b5,
  0x983e5152, 0xee66dfab, 0xa831c66d, 0x2db43210,
  0xb00327c8, 0x98fb213f, 0xbf597fc7, 0xbeef0ee4,
  0xc6e00bf3, 0x3da88fc2, 0xd5a79147, 0x930aa725,
  0x06ca6351, 0xe003826f, 0x14292967, 0x0a0e6e70,
  0x27b70a85, 0x46d22ffc, 0x2e1b2138, 0x5c26c926,
  0x4d2c6dfc, 0x5ac42aed, 0x53380d13, 0x9d95b3df,
  0x650a7354, 0x8baf63de, 0x766a0abb, 0x3c77b2a8,
  0x81c2c92e, 0x47edaee6, 0x92722c85, 0x1482353b,
  0xa2bfe8a1, 0x4cf10364, 0xa81a664b, 0xbc423001,
  0xc24b8b70, 0xd0f89791, 0xc76c51a3, 0x0654be30,
  0xd192e819, 0xd6ef5218, 0xd6990624, 0x5565a910,
  0xf40e3585, 0x5771202a, 0x106aa070, 0x32bbd1b8,
  0x19a4c116, 0xb8d2d0c8, 0x1e376c08, 0x5141ab53,
  0x2748774c, 0xdf8eeb99, 0x34b0bcb5, 0xe19b48a8,
  0x391c0cb3, 0xc5c95a63, 0x4ed8aa4a, 0xe3418acb,
  0x5b9cca4f, 0x7763e373, 0x682e6ff3, 0xd6b2b8a3,
  0x748f82ee, 0x5defb2fc, 0x78a5636f, 0x43172f60,
  0x84c87814, 0xa1f0ab72, 0x8cc70208, 0x1a6439ec,
  0x90befffa, 0x23631e28, 0xa4506ceb, 0xde82bde9,
  0xbef9a3f7, 0xb2c67915, 0xc67178f2, 0xe372532b,
  0xca273ece, 0xea26619c, 0xd186b8c7, 0x21c0c207,
  0xeada7dd6, 0xcde0eb1e, 0xf57d4f7f, 0xee6ed178,
  0x06f067aa, 0x72176fba, 0x0a637dc5, 0xa2c898a6,
  0x113f9804, 0xbef90dae, 0x1b710b35, 0x131c471b,
  0x28db77f5, 0x23047d84, 0x32caab7b, 0x40c72493,
  0x3c9ebe0a, 0x15c9bebc, 0x431d67c4, 0x9c100d4c,
  0x4cc5d4be, 0xcb3e42b6, 0x597f299c, 0xfc657e2a,
  0x5fcb6fab, 0x3ad6faec, 0x6c44198c, 0x4a475817
]

var W = new Array(160)

function Sha512 () {
  this.init()
  this._w = W

  Hash.call(this, 128, 112)
}

inherits(Sha512, Hash)

Sha512.prototype.init = function () {
  this._ah = 0x6a09e667
  this._bh = 0xbb67ae85
  this._ch = 0x3c6ef372
  this._dh = 0xa54ff53a
  this._eh = 0x510e527f
  this._fh = 0x9b05688c
  this._gh = 0x1f83d9ab
  this._hh = 0x5be0cd19

  this._al = 0xf3bcc908
  this._bl = 0x84caa73b
  this._cl = 0xfe94f82b
  this._dl = 0x5f1d36f1
  this._el = 0xade682d1
  this._fl = 0x2b3e6c1f
  this._gl = 0xfb41bd6b
  this._hl = 0x137e2179

  return this
}

function Ch (x, y, z) {
  return z ^ (x & (y ^ z))
}

function maj (x, y, z) {
  return (x & y) | (z & (x | y))
}

function sigma0 (x, xl) {
  return (x >>> 28 | xl << 4) ^ (xl >>> 2 | x << 30) ^ (xl >>> 7 | x << 25)
}

function sigma1 (x, xl) {
  return (x >>> 14 | xl << 18) ^ (x >>> 18 | xl << 14) ^ (xl >>> 9 | x << 23)
}

function Gamma0 (x, xl) {
  return (x >>> 1 | xl << 31) ^ (x >>> 8 | xl << 24) ^ (x >>> 7)
}

function Gamma0l (x, xl) {
  return (x >>> 1 | xl << 31) ^ (x >>> 8 | xl << 24) ^ (x >>> 7 | xl << 25)
}

function Gamma1 (x, xl) {
  return (x >>> 19 | xl << 13) ^ (xl >>> 29 | x << 3) ^ (x >>> 6)
}

function Gamma1l (x, xl) {
  return (x >>> 19 | xl << 13) ^ (xl >>> 29 | x << 3) ^ (x >>> 6 | xl << 26)
}

function getCarry (a, b) {
  return (a >>> 0) < (b >>> 0) ? 1 : 0
}

Sha512.prototype._update = function (M) {
  var W = this._w

  var ah = this._ah | 0
  var bh = this._bh | 0
  var ch = this._ch | 0
  var dh = this._dh | 0
  var eh = this._eh | 0
  var fh = this._fh | 0
  var gh = this._gh | 0
  var hh = this._hh | 0

  var al = this._al | 0
  var bl = this._bl | 0
  var cl = this._cl | 0
  var dl = this._dl | 0
  var el = this._el | 0
  var fl = this._fl | 0
  var gl = this._gl | 0
  var hl = this._hl | 0

  for (var i = 0; i < 32; i += 2) {
    W[i] = M.readInt32BE(i * 4)
    W[i + 1] = M.readInt32BE(i * 4 + 4)
  }
  for (; i < 160; i += 2) {
    var xh = W[i - 15 * 2]
    var xl = W[i - 15 * 2 + 1]
    var gamma0 = Gamma0(xh, xl)
    var gamma0l = Gamma0l(xl, xh)

    xh = W[i - 2 * 2]
    xl = W[i - 2 * 2 + 1]
    var gamma1 = Gamma1(xh, xl)
    var gamma1l = Gamma1l(xl, xh)

    // W[i] = gamma0 + W[i - 7] + gamma1 + W[i - 16]
    var Wi7h = W[i - 7 * 2]
    var Wi7l = W[i - 7 * 2 + 1]

    var Wi16h = W[i - 16 * 2]
    var Wi16l = W[i - 16 * 2 + 1]

    var Wil = (gamma0l + Wi7l) | 0
    var Wih = (gamma0 + Wi7h + getCarry(Wil, gamma0l)) | 0
    Wil = (Wil + gamma1l) | 0
    Wih = (Wih + gamma1 + getCarry(Wil, gamma1l)) | 0
    Wil = (Wil + Wi16l) | 0
    Wih = (Wih + Wi16h + getCarry(Wil, Wi16l)) | 0

    W[i] = Wih
    W[i + 1] = Wil
  }

  for (var j = 0; j < 160; j += 2) {
    Wih = W[j]
    Wil = W[j + 1]

    var majh = maj(ah, bh, ch)
    var majl = maj(al, bl, cl)

    var sigma0h = sigma0(ah, al)
    var sigma0l = sigma0(al, ah)
    var sigma1h = sigma1(eh, el)
    var sigma1l = sigma1(el, eh)

    // t1 = h + sigma1 + ch + K[j] + W[j]
    var Kih = K[j]
    var Kil = K[j + 1]

    var chh = Ch(eh, fh, gh)
    var chl = Ch(el, fl, gl)

    var t1l = (hl + sigma1l) | 0
    var t1h = (hh + sigma1h + getCarry(t1l, hl)) | 0
    t1l = (t1l + chl) | 0
    t1h = (t1h + chh + getCarry(t1l, chl)) | 0
    t1l = (t1l + Kil) | 0
    t1h = (t1h + Kih + getCarry(t1l, Kil)) | 0
    t1l = (t1l + Wil) | 0
    t1h = (t1h + Wih + getCarry(t1l, Wil)) | 0

    // t2 = sigma0 + maj
    var t2l = (sigma0l + majl) | 0
    var t2h = (sigma0h + majh + getCarry(t2l, sigma0l)) | 0

    hh = gh
    hl = gl
    gh = fh
    gl = fl
    fh = eh
    fl = el
    el = (dl + t1l) | 0
    eh = (dh + t1h + getCarry(el, dl)) | 0
    dh = ch
    dl = cl
    ch = bh
    cl = bl
    bh = ah
    bl = al
    al = (t1l + t2l) | 0
    ah = (t1h + t2h + getCarry(al, t1l)) | 0
  }

  this._al = (this._al + al) | 0
  this._bl = (this._bl + bl) | 0
  this._cl = (this._cl + cl) | 0
  this._dl = (this._dl + dl) | 0
  this._el = (this._el + el) | 0
  this._fl = (this._fl + fl) | 0
  this._gl = (this._gl + gl) | 0
  this._hl = (this._hl + hl) | 0

  this._ah = (this._ah + ah + getCarry(this._al, al)) | 0
  this._bh = (this._bh + bh + getCarry(this._bl, bl)) | 0
  this._ch = (this._ch + ch + getCarry(this._cl, cl)) | 0
  this._dh = (this._dh + dh + getCarry(this._dl, dl)) | 0
  this._eh = (this._eh + eh + getCarry(this._el, el)) | 0
  this._fh = (this._fh + fh + getCarry(this._fl, fl)) | 0
  this._gh = (this._gh + gh + getCarry(this._gl, gl)) | 0
  this._hh = (this._hh + hh + getCarry(this._hl, hl)) | 0
}

Sha512.prototype._hash = function () {
  var H = Buffer.allocUnsafe(64)

  function writeInt64BE (h, l, offset) {
    H.writeInt32BE(h, offset)
    H.writeInt32BE(l, offset + 4)
  }

  writeInt64BE(this._ah, this._al, 0)
  writeInt64BE(this._bh, this._bl, 8)
  writeInt64BE(this._ch, this._cl, 16)
  writeInt64BE(this._dh, this._dl, 24)
  writeInt64BE(this._eh, this._el, 32)
  writeInt64BE(this._fh, this._fl, 40)
  writeInt64BE(this._gh, this._gl, 48)
  writeInt64BE(this._hh, this._hl, 56)

  return H
}

module.exports = Sha512

},{"./hash":24,"inherits":14,"safe-buffer":23}],32:[function(require,module,exports){
const {pbkdf2: pbkdf2Async, pbkdf2Sync} = require('pbkdf2')

const promisifiedPbkdf2 = (password, salt, iterations, length, algo) =>
  new Promise((resolveFunction, rejectFunction) => {
    pbkdf2Async(password, salt, iterations, length, algo, (error, response) => {
      if (error) {
        rejectFunction(error)
      }
      resolveFunction(response)
    })
  })

const pbkdf2 = async (password, salt, iterations, length, algo) => {
  try {
    const result = await promisifiedPbkdf2(password, salt, iterations, length, algo)
    return result
  } catch (e) {
    // falback to sync since on Firefox promisifiedPbkdf2 fails for empty password
    return pbkdf2Sync(password, salt, iterations, length, algo)
  }
}

module.exports = pbkdf2

},{"pbkdf2":16}],33:[function(require,module,exports){
(function (Buffer){
const bip39 = require('bip39')

function validateBuffer(input, expectedLength) {
  if (!Buffer.isBuffer(input)) {
    throw new Error('not buffer!')
  }

  if (expectedLength && input.length !== expectedLength) {
    throw new Error('Invalid buffer length')
  }
}

function validateArray(input) {
  if (typeof input !== typeof []) {
    throw new Error('not an array!')
  }
}

function validateDerivationIndex(input) {
  if (!Number.isInteger(input)) {
    throw new Error('invalid derivation index!')
  }
}

function validateString(input) {
  if (typeof input !== typeof 'aa') {
    throw new Error('not a string!')
  }
}

function validateDerivationScheme(input) {
  if (input !== 1 && input !== 2) {
    throw new Error('invalid derivation scheme!')
  }
}

function validateMnemonic(input) {
  if (!bip39.validateMnemonic(input)) {
    const e = new Error('Invalid or unsupported mnemonic format:')
    e.name = 'InvalidArgumentException'
    throw e
  }
}

function validateMnemonicWords(input) {
  const wordlist = bip39.wordlists.EN
  const words = input.split(' ')

  const valid = words.reduce((result, word) => {
    return result && wordlist.indexOf(word) !== -1
  }, true)

  if (!valid) {
    throw new Error('Invalid mnemonic words')
  }
}

function validatePaperWalletMnemonic(input) {
  validateMnemonicWords(input)

  const mnemonicLength = input.split(' ').length

  if (mnemonicLength !== 27) {
    throw Error(
      `Paper Wallet Mnemonic must be 27 words, got ${mnemonicLength} instead`
    )
  }
}

function validateNetworkId(input) {
  if (!Number.isInteger(input) || input < 0 || input > 15) {
    throw Error(
      'Network id must be an integer between 0 and 15'
    )
  }
}

function validateUint32(input) {
  if (!Number.isInteger(input) || input < 0 || input >= Math.pow(2, 32)) {
    throw Error(
      'Value must be uint32'
    )
  }
}

module.exports = {
  validateBuffer,
  validateArray,
  validateString,
  validateDerivationIndex,
  validateDerivationScheme,
  validateMnemonic,
  validateMnemonicWords,
  validatePaperWalletMnemonic,
  validateNetworkId,
  validateUint32
}

}).call(this,{"isBuffer":require("../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js")})
},{"../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js":42,"bip39":8}],34:[function(require,module,exports){

},{}],35:[function(require,module,exports){
'use strict'

exports.byteLength = byteLength
exports.toByteArray = toByteArray
exports.fromByteArray = fromByteArray

var lookup = []
var revLookup = []
var Arr = typeof Uint8Array !== 'undefined' ? Uint8Array : Array

var code = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
for (var i = 0, len = code.length; i < len; ++i) {
  lookup[i] = code[i]
  revLookup[code.charCodeAt(i)] = i
}

// Support decoding URL-safe base64 strings, as Node.js does.
// See: https://en.wikipedia.org/wiki/Base64#URL_applications
revLookup['-'.charCodeAt(0)] = 62
revLookup['_'.charCodeAt(0)] = 63

function getLens (b64) {
  var len = b64.length

  if (len % 4 > 0) {
    throw new Error('Invalid string. Length must be a multiple of 4')
  }

  // Trim off extra bytes after placeholder bytes are found
  // See: https://github.com/beatgammit/base64-js/issues/42
  var validLen = b64.indexOf('=')
  if (validLen === -1) validLen = len

  var placeHoldersLen = validLen === len
    ? 0
    : 4 - (validLen % 4)

  return [validLen, placeHoldersLen]
}

// base64 is 4/3 + up to two characters of the original data
function byteLength (b64) {
  var lens = getLens(b64)
  var validLen = lens[0]
  var placeHoldersLen = lens[1]
  return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
}

function _byteLength (b64, validLen, placeHoldersLen) {
  return ((validLen + placeHoldersLen) * 3 / 4) - placeHoldersLen
}

function toByteArray (b64) {
  var tmp
  var lens = getLens(b64)
  var validLen = lens[0]
  var placeHoldersLen = lens[1]

  var arr = new Arr(_byteLength(b64, validLen, placeHoldersLen))

  var curByte = 0

  // if there are placeholders, only get up to the last complete 4 chars
  var len = placeHoldersLen > 0
    ? validLen - 4
    : validLen

  var i
  for (i = 0; i < len; i += 4) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 18) |
      (revLookup[b64.charCodeAt(i + 1)] << 12) |
      (revLookup[b64.charCodeAt(i + 2)] << 6) |
      revLookup[b64.charCodeAt(i + 3)]
    arr[curByte++] = (tmp >> 16) & 0xFF
    arr[curByte++] = (tmp >> 8) & 0xFF
    arr[curByte++] = tmp & 0xFF
  }

  if (placeHoldersLen === 2) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 2) |
      (revLookup[b64.charCodeAt(i + 1)] >> 4)
    arr[curByte++] = tmp & 0xFF
  }

  if (placeHoldersLen === 1) {
    tmp =
      (revLookup[b64.charCodeAt(i)] << 10) |
      (revLookup[b64.charCodeAt(i + 1)] << 4) |
      (revLookup[b64.charCodeAt(i + 2)] >> 2)
    arr[curByte++] = (tmp >> 8) & 0xFF
    arr[curByte++] = tmp & 0xFF
  }

  return arr
}

function tripletToBase64 (num) {
  return lookup[num >> 18 & 0x3F] +
    lookup[num >> 12 & 0x3F] +
    lookup[num >> 6 & 0x3F] +
    lookup[num & 0x3F]
}

function encodeChunk (uint8, start, end) {
  var tmp
  var output = []
  for (var i = start; i < end; i += 3) {
    tmp =
      ((uint8[i] << 16) & 0xFF0000) +
      ((uint8[i + 1] << 8) & 0xFF00) +
      (uint8[i + 2] & 0xFF)
    output.push(tripletToBase64(tmp))
  }
  return output.join('')
}

function fromByteArray (uint8) {
  var tmp
  var len = uint8.length
  var extraBytes = len % 3 // if we have 1 byte left, pad 2 bytes
  var parts = []
  var maxChunkLength = 16383 // must be multiple of 3

  // go through the array every three bytes, we'll deal with trailing stuff later
  for (var i = 0, len2 = len - extraBytes; i < len2; i += maxChunkLength) {
    parts.push(encodeChunk(
      uint8, i, (i + maxChunkLength) > len2 ? len2 : (i + maxChunkLength)
    ))
  }

  // pad the end with zeros, but make sure to not forget the extra bytes
  if (extraBytes === 1) {
    tmp = uint8[len - 1]
    parts.push(
      lookup[tmp >> 2] +
      lookup[(tmp << 4) & 0x3F] +
      '=='
    )
  } else if (extraBytes === 2) {
    tmp = (uint8[len - 2] << 8) + uint8[len - 1]
    parts.push(
      lookup[tmp >> 10] +
      lookup[(tmp >> 4) & 0x3F] +
      lookup[(tmp << 2) & 0x3F] +
      '='
    )
  }

  return parts.join('')
}

},{}],36:[function(require,module,exports){
arguments[4][34][0].apply(exports,arguments)
},{"dup":34}],37:[function(require,module,exports){
(function (Buffer){
/*!
 * The buffer module from node.js, for the browser.
 *
 * @author   Feross Aboukhadijeh <https://feross.org>
 * @license  MIT
 */
/* eslint-disable no-proto */

'use strict'

var base64 = require('base64-js')
var ieee754 = require('ieee754')

exports.Buffer = Buffer
exports.SlowBuffer = SlowBuffer
exports.INSPECT_MAX_BYTES = 50

var K_MAX_LENGTH = 0x7fffffff
exports.kMaxLength = K_MAX_LENGTH

/**
 * If `Buffer.TYPED_ARRAY_SUPPORT`:
 *   === true    Use Uint8Array implementation (fastest)
 *   === false   Print warning and recommend using `buffer` v4.x which has an Object
 *               implementation (most compatible, even IE6)
 *
 * Browsers that support typed arrays are IE 10+, Firefox 4+, Chrome 7+, Safari 5.1+,
 * Opera 11.6+, iOS 4.2+.
 *
 * We report that the browser does not support typed arrays if the are not subclassable
 * using __proto__. Firefox 4-29 lacks support for adding new properties to `Uint8Array`
 * (See: https://bugzilla.mozilla.org/show_bug.cgi?id=695438). IE 10 lacks support
 * for __proto__ and has a buggy typed array implementation.
 */
Buffer.TYPED_ARRAY_SUPPORT = typedArraySupport()

if (!Buffer.TYPED_ARRAY_SUPPORT && typeof console !== 'undefined' &&
    typeof console.error === 'function') {
  console.error(
    'This browser lacks typed array (Uint8Array) support which is required by ' +
    '`buffer` v5.x. Use `buffer` v4.x if you require old browser support.'
  )
}

function typedArraySupport () {
  // Can typed array instances can be augmented?
  try {
    var arr = new Uint8Array(1)
    arr.__proto__ = { __proto__: Uint8Array.prototype, foo: function () { return 42 } }
    return arr.foo() === 42
  } catch (e) {
    return false
  }
}

Object.defineProperty(Buffer.prototype, 'parent', {
  enumerable: true,
  get: function () {
    if (!Buffer.isBuffer(this)) return undefined
    return this.buffer
  }
})

Object.defineProperty(Buffer.prototype, 'offset', {
  enumerable: true,
  get: function () {
    if (!Buffer.isBuffer(this)) return undefined
    return this.byteOffset
  }
})

function createBuffer (length) {
  if (length > K_MAX_LENGTH) {
    throw new RangeError('The value "' + length + '" is invalid for option "size"')
  }
  // Return an augmented `Uint8Array` instance
  var buf = new Uint8Array(length)
  buf.__proto__ = Buffer.prototype
  return buf
}

/**
 * The Buffer constructor returns instances of `Uint8Array` that have their
 * prototype changed to `Buffer.prototype`. Furthermore, `Buffer` is a subclass of
 * `Uint8Array`, so the returned instances will have all the node `Buffer` methods
 * and the `Uint8Array` methods. Square bracket notation works as expected -- it
 * returns a single octet.
 *
 * The `Uint8Array` prototype remains unmodified.
 */

function Buffer (arg, encodingOrOffset, length) {
  // Common case.
  if (typeof arg === 'number') {
    if (typeof encodingOrOffset === 'string') {
      throw new TypeError(
        'The "string" argument must be of type string. Received type number'
      )
    }
    return allocUnsafe(arg)
  }
  return from(arg, encodingOrOffset, length)
}

// Fix subarray() in ES2016. See: https://github.com/feross/buffer/pull/97
if (typeof Symbol !== 'undefined' && Symbol.species != null &&
    Buffer[Symbol.species] === Buffer) {
  Object.defineProperty(Buffer, Symbol.species, {
    value: null,
    configurable: true,
    enumerable: false,
    writable: false
  })
}

Buffer.poolSize = 8192 // not used by this implementation

function from (value, encodingOrOffset, length) {
  if (typeof value === 'string') {
    return fromString(value, encodingOrOffset)
  }

  if (ArrayBuffer.isView(value)) {
    return fromArrayLike(value)
  }

  if (value == null) {
    throw TypeError(
      'The first argument must be one of type string, Buffer, ArrayBuffer, Array, ' +
      'or Array-like Object. Received type ' + (typeof value)
    )
  }

  if (isInstance(value, ArrayBuffer) ||
      (value && isInstance(value.buffer, ArrayBuffer))) {
    return fromArrayBuffer(value, encodingOrOffset, length)
  }

  if (typeof value === 'number') {
    throw new TypeError(
      'The "value" argument must not be of type number. Received type number'
    )
  }

  var valueOf = value.valueOf && value.valueOf()
  if (valueOf != null && valueOf !== value) {
    return Buffer.from(valueOf, encodingOrOffset, length)
  }

  var b = fromObject(value)
  if (b) return b

  if (typeof Symbol !== 'undefined' && Symbol.toPrimitive != null &&
      typeof value[Symbol.toPrimitive] === 'function') {
    return Buffer.from(
      value[Symbol.toPrimitive]('string'), encodingOrOffset, length
    )
  }

  throw new TypeError(
    'The first argument must be one of type string, Buffer, ArrayBuffer, Array, ' +
    'or Array-like Object. Received type ' + (typeof value)
  )
}

/**
 * Functionally equivalent to Buffer(arg, encoding) but throws a TypeError
 * if value is a number.
 * Buffer.from(str[, encoding])
 * Buffer.from(array)
 * Buffer.from(buffer)
 * Buffer.from(arrayBuffer[, byteOffset[, length]])
 **/
Buffer.from = function (value, encodingOrOffset, length) {
  return from(value, encodingOrOffset, length)
}

// Note: Change prototype *after* Buffer.from is defined to workaround Chrome bug:
// https://github.com/feross/buffer/pull/148
Buffer.prototype.__proto__ = Uint8Array.prototype
Buffer.__proto__ = Uint8Array

function assertSize (size) {
  if (typeof size !== 'number') {
    throw new TypeError('"size" argument must be of type number')
  } else if (size < 0) {
    throw new RangeError('The value "' + size + '" is invalid for option "size"')
  }
}

function alloc (size, fill, encoding) {
  assertSize(size)
  if (size <= 0) {
    return createBuffer(size)
  }
  if (fill !== undefined) {
    // Only pay attention to encoding if it's a string. This
    // prevents accidentally sending in a number that would
    // be interpretted as a start offset.
    return typeof encoding === 'string'
      ? createBuffer(size).fill(fill, encoding)
      : createBuffer(size).fill(fill)
  }
  return createBuffer(size)
}

/**
 * Creates a new filled Buffer instance.
 * alloc(size[, fill[, encoding]])
 **/
Buffer.alloc = function (size, fill, encoding) {
  return alloc(size, fill, encoding)
}

function allocUnsafe (size) {
  assertSize(size)
  return createBuffer(size < 0 ? 0 : checked(size) | 0)
}

/**
 * Equivalent to Buffer(num), by default creates a non-zero-filled Buffer instance.
 * */
Buffer.allocUnsafe = function (size) {
  return allocUnsafe(size)
}
/**
 * Equivalent to SlowBuffer(num), by default creates a non-zero-filled Buffer instance.
 */
Buffer.allocUnsafeSlow = function (size) {
  return allocUnsafe(size)
}

function fromString (string, encoding) {
  if (typeof encoding !== 'string' || encoding === '') {
    encoding = 'utf8'
  }

  if (!Buffer.isEncoding(encoding)) {
    throw new TypeError('Unknown encoding: ' + encoding)
  }

  var length = byteLength(string, encoding) | 0
  var buf = createBuffer(length)

  var actual = buf.write(string, encoding)

  if (actual !== length) {
    // Writing a hex string, for example, that contains invalid characters will
    // cause everything after the first invalid character to be ignored. (e.g.
    // 'abxxcd' will be treated as 'ab')
    buf = buf.slice(0, actual)
  }

  return buf
}

function fromArrayLike (array) {
  var length = array.length < 0 ? 0 : checked(array.length) | 0
  var buf = createBuffer(length)
  for (var i = 0; i < length; i += 1) {
    buf[i] = array[i] & 255
  }
  return buf
}

function fromArrayBuffer (array, byteOffset, length) {
  if (byteOffset < 0 || array.byteLength < byteOffset) {
    throw new RangeError('"offset" is outside of buffer bounds')
  }

  if (array.byteLength < byteOffset + (length || 0)) {
    throw new RangeError('"length" is outside of buffer bounds')
  }

  var buf
  if (byteOffset === undefined && length === undefined) {
    buf = new Uint8Array(array)
  } else if (length === undefined) {
    buf = new Uint8Array(array, byteOffset)
  } else {
    buf = new Uint8Array(array, byteOffset, length)
  }

  // Return an augmented `Uint8Array` instance
  buf.__proto__ = Buffer.prototype
  return buf
}

function fromObject (obj) {
  if (Buffer.isBuffer(obj)) {
    var len = checked(obj.length) | 0
    var buf = createBuffer(len)

    if (buf.length === 0) {
      return buf
    }

    obj.copy(buf, 0, 0, len)
    return buf
  }

  if (obj.length !== undefined) {
    if (typeof obj.length !== 'number' || numberIsNaN(obj.length)) {
      return createBuffer(0)
    }
    return fromArrayLike(obj)
  }

  if (obj.type === 'Buffer' && Array.isArray(obj.data)) {
    return fromArrayLike(obj.data)
  }
}

function checked (length) {
  // Note: cannot use `length < K_MAX_LENGTH` here because that fails when
  // length is NaN (which is otherwise coerced to zero.)
  if (length >= K_MAX_LENGTH) {
    throw new RangeError('Attempt to allocate Buffer larger than maximum ' +
                         'size: 0x' + K_MAX_LENGTH.toString(16) + ' bytes')
  }
  return length | 0
}

function SlowBuffer (length) {
  if (+length != length) { // eslint-disable-line eqeqeq
    length = 0
  }
  return Buffer.alloc(+length)
}

Buffer.isBuffer = function isBuffer (b) {
  return b != null && b._isBuffer === true &&
    b !== Buffer.prototype // so Buffer.isBuffer(Buffer.prototype) will be false
}

Buffer.compare = function compare (a, b) {
  if (isInstance(a, Uint8Array)) a = Buffer.from(a, a.offset, a.byteLength)
  if (isInstance(b, Uint8Array)) b = Buffer.from(b, b.offset, b.byteLength)
  if (!Buffer.isBuffer(a) || !Buffer.isBuffer(b)) {
    throw new TypeError(
      'The "buf1", "buf2" arguments must be one of type Buffer or Uint8Array'
    )
  }

  if (a === b) return 0

  var x = a.length
  var y = b.length

  for (var i = 0, len = Math.min(x, y); i < len; ++i) {
    if (a[i] !== b[i]) {
      x = a[i]
      y = b[i]
      break
    }
  }

  if (x < y) return -1
  if (y < x) return 1
  return 0
}

Buffer.isEncoding = function isEncoding (encoding) {
  switch (String(encoding).toLowerCase()) {
    case 'hex':
    case 'utf8':
    case 'utf-8':
    case 'ascii':
    case 'latin1':
    case 'binary':
    case 'base64':
    case 'ucs2':
    case 'ucs-2':
    case 'utf16le':
    case 'utf-16le':
      return true
    default:
      return false
  }
}

Buffer.concat = function concat (list, length) {
  if (!Array.isArray(list)) {
    throw new TypeError('"list" argument must be an Array of Buffers')
  }

  if (list.length === 0) {
    return Buffer.alloc(0)
  }

  var i
  if (length === undefined) {
    length = 0
    for (i = 0; i < list.length; ++i) {
      length += list[i].length
    }
  }

  var buffer = Buffer.allocUnsafe(length)
  var pos = 0
  for (i = 0; i < list.length; ++i) {
    var buf = list[i]
    if (isInstance(buf, Uint8Array)) {
      buf = Buffer.from(buf)
    }
    if (!Buffer.isBuffer(buf)) {
      throw new TypeError('"list" argument must be an Array of Buffers')
    }
    buf.copy(buffer, pos)
    pos += buf.length
  }
  return buffer
}

function byteLength (string, encoding) {
  if (Buffer.isBuffer(string)) {
    return string.length
  }
  if (ArrayBuffer.isView(string) || isInstance(string, ArrayBuffer)) {
    return string.byteLength
  }
  if (typeof string !== 'string') {
    throw new TypeError(
      'The "string" argument must be one of type string, Buffer, or ArrayBuffer. ' +
      'Received type ' + typeof string
    )
  }

  var len = string.length
  var mustMatch = (arguments.length > 2 && arguments[2] === true)
  if (!mustMatch && len === 0) return 0

  // Use a for loop to avoid recursion
  var loweredCase = false
  for (;;) {
    switch (encoding) {
      case 'ascii':
      case 'latin1':
      case 'binary':
        return len
      case 'utf8':
      case 'utf-8':
        return utf8ToBytes(string).length
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return len * 2
      case 'hex':
        return len >>> 1
      case 'base64':
        return base64ToBytes(string).length
      default:
        if (loweredCase) {
          return mustMatch ? -1 : utf8ToBytes(string).length // assume utf8
        }
        encoding = ('' + encoding).toLowerCase()
        loweredCase = true
    }
  }
}
Buffer.byteLength = byteLength

function slowToString (encoding, start, end) {
  var loweredCase = false

  // No need to verify that "this.length <= MAX_UINT32" since it's a read-only
  // property of a typed array.

  // This behaves neither like String nor Uint8Array in that we set start/end
  // to their upper/lower bounds if the value passed is out of range.
  // undefined is handled specially as per ECMA-262 6th Edition,
  // Section 13.3.3.7 Runtime Semantics: KeyedBindingInitialization.
  if (start === undefined || start < 0) {
    start = 0
  }
  // Return early if start > this.length. Done here to prevent potential uint32
  // coercion fail below.
  if (start > this.length) {
    return ''
  }

  if (end === undefined || end > this.length) {
    end = this.length
  }

  if (end <= 0) {
    return ''
  }

  // Force coersion to uint32. This will also coerce falsey/NaN values to 0.
  end >>>= 0
  start >>>= 0

  if (end <= start) {
    return ''
  }

  if (!encoding) encoding = 'utf8'

  while (true) {
    switch (encoding) {
      case 'hex':
        return hexSlice(this, start, end)

      case 'utf8':
      case 'utf-8':
        return utf8Slice(this, start, end)

      case 'ascii':
        return asciiSlice(this, start, end)

      case 'latin1':
      case 'binary':
        return latin1Slice(this, start, end)

      case 'base64':
        return base64Slice(this, start, end)

      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return utf16leSlice(this, start, end)

      default:
        if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
        encoding = (encoding + '').toLowerCase()
        loweredCase = true
    }
  }
}

// This property is used by `Buffer.isBuffer` (and the `is-buffer` npm package)
// to detect a Buffer instance. It's not possible to use `instanceof Buffer`
// reliably in a browserify context because there could be multiple different
// copies of the 'buffer' package in use. This method works even for Buffer
// instances that were created from another copy of the `buffer` package.
// See: https://github.com/feross/buffer/issues/154
Buffer.prototype._isBuffer = true

function swap (b, n, m) {
  var i = b[n]
  b[n] = b[m]
  b[m] = i
}

Buffer.prototype.swap16 = function swap16 () {
  var len = this.length
  if (len % 2 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 16-bits')
  }
  for (var i = 0; i < len; i += 2) {
    swap(this, i, i + 1)
  }
  return this
}

Buffer.prototype.swap32 = function swap32 () {
  var len = this.length
  if (len % 4 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 32-bits')
  }
  for (var i = 0; i < len; i += 4) {
    swap(this, i, i + 3)
    swap(this, i + 1, i + 2)
  }
  return this
}

Buffer.prototype.swap64 = function swap64 () {
  var len = this.length
  if (len % 8 !== 0) {
    throw new RangeError('Buffer size must be a multiple of 64-bits')
  }
  for (var i = 0; i < len; i += 8) {
    swap(this, i, i + 7)
    swap(this, i + 1, i + 6)
    swap(this, i + 2, i + 5)
    swap(this, i + 3, i + 4)
  }
  return this
}

Buffer.prototype.toString = function toString () {
  var length = this.length
  if (length === 0) return ''
  if (arguments.length === 0) return utf8Slice(this, 0, length)
  return slowToString.apply(this, arguments)
}

Buffer.prototype.toLocaleString = Buffer.prototype.toString

Buffer.prototype.equals = function equals (b) {
  if (!Buffer.isBuffer(b)) throw new TypeError('Argument must be a Buffer')
  if (this === b) return true
  return Buffer.compare(this, b) === 0
}

Buffer.prototype.inspect = function inspect () {
  var str = ''
  var max = exports.INSPECT_MAX_BYTES
  str = this.toString('hex', 0, max).replace(/(.{2})/g, '$1 ').trim()
  if (this.length > max) str += ' ... '
  return '<Buffer ' + str + '>'
}

Buffer.prototype.compare = function compare (target, start, end, thisStart, thisEnd) {
  if (isInstance(target, Uint8Array)) {
    target = Buffer.from(target, target.offset, target.byteLength)
  }
  if (!Buffer.isBuffer(target)) {
    throw new TypeError(
      'The "target" argument must be one of type Buffer or Uint8Array. ' +
      'Received type ' + (typeof target)
    )
  }

  if (start === undefined) {
    start = 0
  }
  if (end === undefined) {
    end = target ? target.length : 0
  }
  if (thisStart === undefined) {
    thisStart = 0
  }
  if (thisEnd === undefined) {
    thisEnd = this.length
  }

  if (start < 0 || end > target.length || thisStart < 0 || thisEnd > this.length) {
    throw new RangeError('out of range index')
  }

  if (thisStart >= thisEnd && start >= end) {
    return 0
  }
  if (thisStart >= thisEnd) {
    return -1
  }
  if (start >= end) {
    return 1
  }

  start >>>= 0
  end >>>= 0
  thisStart >>>= 0
  thisEnd >>>= 0

  if (this === target) return 0

  var x = thisEnd - thisStart
  var y = end - start
  var len = Math.min(x, y)

  var thisCopy = this.slice(thisStart, thisEnd)
  var targetCopy = target.slice(start, end)

  for (var i = 0; i < len; ++i) {
    if (thisCopy[i] !== targetCopy[i]) {
      x = thisCopy[i]
      y = targetCopy[i]
      break
    }
  }

  if (x < y) return -1
  if (y < x) return 1
  return 0
}

// Finds either the first index of `val` in `buffer` at offset >= `byteOffset`,
// OR the last index of `val` in `buffer` at offset <= `byteOffset`.
//
// Arguments:
// - buffer - a Buffer to search
// - val - a string, Buffer, or number
// - byteOffset - an index into `buffer`; will be clamped to an int32
// - encoding - an optional encoding, relevant is val is a string
// - dir - true for indexOf, false for lastIndexOf
function bidirectionalIndexOf (buffer, val, byteOffset, encoding, dir) {
  // Empty buffer means no match
  if (buffer.length === 0) return -1

  // Normalize byteOffset
  if (typeof byteOffset === 'string') {
    encoding = byteOffset
    byteOffset = 0
  } else if (byteOffset > 0x7fffffff) {
    byteOffset = 0x7fffffff
  } else if (byteOffset < -0x80000000) {
    byteOffset = -0x80000000
  }
  byteOffset = +byteOffset // Coerce to Number.
  if (numberIsNaN(byteOffset)) {
    // byteOffset: it it's undefined, null, NaN, "foo", etc, search whole buffer
    byteOffset = dir ? 0 : (buffer.length - 1)
  }

  // Normalize byteOffset: negative offsets start from the end of the buffer
  if (byteOffset < 0) byteOffset = buffer.length + byteOffset
  if (byteOffset >= buffer.length) {
    if (dir) return -1
    else byteOffset = buffer.length - 1
  } else if (byteOffset < 0) {
    if (dir) byteOffset = 0
    else return -1
  }

  // Normalize val
  if (typeof val === 'string') {
    val = Buffer.from(val, encoding)
  }

  // Finally, search either indexOf (if dir is true) or lastIndexOf
  if (Buffer.isBuffer(val)) {
    // Special case: looking for empty string/buffer always fails
    if (val.length === 0) {
      return -1
    }
    return arrayIndexOf(buffer, val, byteOffset, encoding, dir)
  } else if (typeof val === 'number') {
    val = val & 0xFF // Search for a byte value [0-255]
    if (typeof Uint8Array.prototype.indexOf === 'function') {
      if (dir) {
        return Uint8Array.prototype.indexOf.call(buffer, val, byteOffset)
      } else {
        return Uint8Array.prototype.lastIndexOf.call(buffer, val, byteOffset)
      }
    }
    return arrayIndexOf(buffer, [ val ], byteOffset, encoding, dir)
  }

  throw new TypeError('val must be string, number or Buffer')
}

function arrayIndexOf (arr, val, byteOffset, encoding, dir) {
  var indexSize = 1
  var arrLength = arr.length
  var valLength = val.length

  if (encoding !== undefined) {
    encoding = String(encoding).toLowerCase()
    if (encoding === 'ucs2' || encoding === 'ucs-2' ||
        encoding === 'utf16le' || encoding === 'utf-16le') {
      if (arr.length < 2 || val.length < 2) {
        return -1
      }
      indexSize = 2
      arrLength /= 2
      valLength /= 2
      byteOffset /= 2
    }
  }

  function read (buf, i) {
    if (indexSize === 1) {
      return buf[i]
    } else {
      return buf.readUInt16BE(i * indexSize)
    }
  }

  var i
  if (dir) {
    var foundIndex = -1
    for (i = byteOffset; i < arrLength; i++) {
      if (read(arr, i) === read(val, foundIndex === -1 ? 0 : i - foundIndex)) {
        if (foundIndex === -1) foundIndex = i
        if (i - foundIndex + 1 === valLength) return foundIndex * indexSize
      } else {
        if (foundIndex !== -1) i -= i - foundIndex
        foundIndex = -1
      }
    }
  } else {
    if (byteOffset + valLength > arrLength) byteOffset = arrLength - valLength
    for (i = byteOffset; i >= 0; i--) {
      var found = true
      for (var j = 0; j < valLength; j++) {
        if (read(arr, i + j) !== read(val, j)) {
          found = false
          break
        }
      }
      if (found) return i
    }
  }

  return -1
}

Buffer.prototype.includes = function includes (val, byteOffset, encoding) {
  return this.indexOf(val, byteOffset, encoding) !== -1
}

Buffer.prototype.indexOf = function indexOf (val, byteOffset, encoding) {
  return bidirectionalIndexOf(this, val, byteOffset, encoding, true)
}

Buffer.prototype.lastIndexOf = function lastIndexOf (val, byteOffset, encoding) {
  return bidirectionalIndexOf(this, val, byteOffset, encoding, false)
}

function hexWrite (buf, string, offset, length) {
  offset = Number(offset) || 0
  var remaining = buf.length - offset
  if (!length) {
    length = remaining
  } else {
    length = Number(length)
    if (length > remaining) {
      length = remaining
    }
  }

  var strLen = string.length

  if (length > strLen / 2) {
    length = strLen / 2
  }
  for (var i = 0; i < length; ++i) {
    var parsed = parseInt(string.substr(i * 2, 2), 16)
    if (numberIsNaN(parsed)) return i
    buf[offset + i] = parsed
  }
  return i
}

function utf8Write (buf, string, offset, length) {
  return blitBuffer(utf8ToBytes(string, buf.length - offset), buf, offset, length)
}

function asciiWrite (buf, string, offset, length) {
  return blitBuffer(asciiToBytes(string), buf, offset, length)
}

function latin1Write (buf, string, offset, length) {
  return asciiWrite(buf, string, offset, length)
}

function base64Write (buf, string, offset, length) {
  return blitBuffer(base64ToBytes(string), buf, offset, length)
}

function ucs2Write (buf, string, offset, length) {
  return blitBuffer(utf16leToBytes(string, buf.length - offset), buf, offset, length)
}

Buffer.prototype.write = function write (string, offset, length, encoding) {
  // Buffer#write(string)
  if (offset === undefined) {
    encoding = 'utf8'
    length = this.length
    offset = 0
  // Buffer#write(string, encoding)
  } else if (length === undefined && typeof offset === 'string') {
    encoding = offset
    length = this.length
    offset = 0
  // Buffer#write(string, offset[, length][, encoding])
  } else if (isFinite(offset)) {
    offset = offset >>> 0
    if (isFinite(length)) {
      length = length >>> 0
      if (encoding === undefined) encoding = 'utf8'
    } else {
      encoding = length
      length = undefined
    }
  } else {
    throw new Error(
      'Buffer.write(string, encoding, offset[, length]) is no longer supported'
    )
  }

  var remaining = this.length - offset
  if (length === undefined || length > remaining) length = remaining

  if ((string.length > 0 && (length < 0 || offset < 0)) || offset > this.length) {
    throw new RangeError('Attempt to write outside buffer bounds')
  }

  if (!encoding) encoding = 'utf8'

  var loweredCase = false
  for (;;) {
    switch (encoding) {
      case 'hex':
        return hexWrite(this, string, offset, length)

      case 'utf8':
      case 'utf-8':
        return utf8Write(this, string, offset, length)

      case 'ascii':
        return asciiWrite(this, string, offset, length)

      case 'latin1':
      case 'binary':
        return latin1Write(this, string, offset, length)

      case 'base64':
        // Warning: maxLength not taken into account in base64Write
        return base64Write(this, string, offset, length)

      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return ucs2Write(this, string, offset, length)

      default:
        if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
        encoding = ('' + encoding).toLowerCase()
        loweredCase = true
    }
  }
}

Buffer.prototype.toJSON = function toJSON () {
  return {
    type: 'Buffer',
    data: Array.prototype.slice.call(this._arr || this, 0)
  }
}

function base64Slice (buf, start, end) {
  if (start === 0 && end === buf.length) {
    return base64.fromByteArray(buf)
  } else {
    return base64.fromByteArray(buf.slice(start, end))
  }
}

function utf8Slice (buf, start, end) {
  end = Math.min(buf.length, end)
  var res = []

  var i = start
  while (i < end) {
    var firstByte = buf[i]
    var codePoint = null
    var bytesPerSequence = (firstByte > 0xEF) ? 4
      : (firstByte > 0xDF) ? 3
        : (firstByte > 0xBF) ? 2
          : 1

    if (i + bytesPerSequence <= end) {
      var secondByte, thirdByte, fourthByte, tempCodePoint

      switch (bytesPerSequence) {
        case 1:
          if (firstByte < 0x80) {
            codePoint = firstByte
          }
          break
        case 2:
          secondByte = buf[i + 1]
          if ((secondByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0x1F) << 0x6 | (secondByte & 0x3F)
            if (tempCodePoint > 0x7F) {
              codePoint = tempCodePoint
            }
          }
          break
        case 3:
          secondByte = buf[i + 1]
          thirdByte = buf[i + 2]
          if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0xF) << 0xC | (secondByte & 0x3F) << 0x6 | (thirdByte & 0x3F)
            if (tempCodePoint > 0x7FF && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) {
              codePoint = tempCodePoint
            }
          }
          break
        case 4:
          secondByte = buf[i + 1]
          thirdByte = buf[i + 2]
          fourthByte = buf[i + 3]
          if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80 && (fourthByte & 0xC0) === 0x80) {
            tempCodePoint = (firstByte & 0xF) << 0x12 | (secondByte & 0x3F) << 0xC | (thirdByte & 0x3F) << 0x6 | (fourthByte & 0x3F)
            if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) {
              codePoint = tempCodePoint
            }
          }
      }
    }

    if (codePoint === null) {
      // we did not generate a valid codePoint so insert a
      // replacement char (U+FFFD) and advance only 1 byte
      codePoint = 0xFFFD
      bytesPerSequence = 1
    } else if (codePoint > 0xFFFF) {
      // encode to utf16 (surrogate pair dance)
      codePoint -= 0x10000
      res.push(codePoint >>> 10 & 0x3FF | 0xD800)
      codePoint = 0xDC00 | codePoint & 0x3FF
    }

    res.push(codePoint)
    i += bytesPerSequence
  }

  return decodeCodePointsArray(res)
}

// Based on http://stackoverflow.com/a/22747272/680742, the browser with
// the lowest limit is Chrome, with 0x10000 args.
// We go 1 magnitude less, for safety
var MAX_ARGUMENTS_LENGTH = 0x1000

function decodeCodePointsArray (codePoints) {
  var len = codePoints.length
  if (len <= MAX_ARGUMENTS_LENGTH) {
    return String.fromCharCode.apply(String, codePoints) // avoid extra slice()
  }

  // Decode in chunks to avoid "call stack size exceeded".
  var res = ''
  var i = 0
  while (i < len) {
    res += String.fromCharCode.apply(
      String,
      codePoints.slice(i, i += MAX_ARGUMENTS_LENGTH)
    )
  }
  return res
}

function asciiSlice (buf, start, end) {
  var ret = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i] & 0x7F)
  }
  return ret
}

function latin1Slice (buf, start, end) {
  var ret = ''
  end = Math.min(buf.length, end)

  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i])
  }
  return ret
}

function hexSlice (buf, start, end) {
  var len = buf.length

  if (!start || start < 0) start = 0
  if (!end || end < 0 || end > len) end = len

  var out = ''
  for (var i = start; i < end; ++i) {
    out += toHex(buf[i])
  }
  return out
}

function utf16leSlice (buf, start, end) {
  var bytes = buf.slice(start, end)
  var res = ''
  for (var i = 0; i < bytes.length; i += 2) {
    res += String.fromCharCode(bytes[i] + (bytes[i + 1] * 256))
  }
  return res
}

Buffer.prototype.slice = function slice (start, end) {
  var len = this.length
  start = ~~start
  end = end === undefined ? len : ~~end

  if (start < 0) {
    start += len
    if (start < 0) start = 0
  } else if (start > len) {
    start = len
  }

  if (end < 0) {
    end += len
    if (end < 0) end = 0
  } else if (end > len) {
    end = len
  }

  if (end < start) end = start

  var newBuf = this.subarray(start, end)
  // Return an augmented `Uint8Array` instance
  newBuf.__proto__ = Buffer.prototype
  return newBuf
}

/*
 * Need to make sure that buffer isn't trying to write out of bounds.
 */
function checkOffset (offset, ext, length) {
  if ((offset % 1) !== 0 || offset < 0) throw new RangeError('offset is not uint')
  if (offset + ext > length) throw new RangeError('Trying to access beyond buffer length')
}

Buffer.prototype.readUIntLE = function readUIntLE (offset, byteLength, noAssert) {
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var val = this[offset]
  var mul = 1
  var i = 0
  while (++i < byteLength && (mul *= 0x100)) {
    val += this[offset + i] * mul
  }

  return val
}

Buffer.prototype.readUIntBE = function readUIntBE (offset, byteLength, noAssert) {
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) {
    checkOffset(offset, byteLength, this.length)
  }

  var val = this[offset + --byteLength]
  var mul = 1
  while (byteLength > 0 && (mul *= 0x100)) {
    val += this[offset + --byteLength] * mul
  }

  return val
}

Buffer.prototype.readUInt8 = function readUInt8 (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 1, this.length)
  return this[offset]
}

Buffer.prototype.readUInt16LE = function readUInt16LE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 2, this.length)
  return this[offset] | (this[offset + 1] << 8)
}

Buffer.prototype.readUInt16BE = function readUInt16BE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 2, this.length)
  return (this[offset] << 8) | this[offset + 1]
}

Buffer.prototype.readUInt32LE = function readUInt32LE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)

  return ((this[offset]) |
      (this[offset + 1] << 8) |
      (this[offset + 2] << 16)) +
      (this[offset + 3] * 0x1000000)
}

Buffer.prototype.readUInt32BE = function readUInt32BE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset] * 0x1000000) +
    ((this[offset + 1] << 16) |
    (this[offset + 2] << 8) |
    this[offset + 3])
}

Buffer.prototype.readIntLE = function readIntLE (offset, byteLength, noAssert) {
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var val = this[offset]
  var mul = 1
  var i = 0
  while (++i < byteLength && (mul *= 0x100)) {
    val += this[offset + i] * mul
  }
  mul *= 0x80

  if (val >= mul) val -= Math.pow(2, 8 * byteLength)

  return val
}

Buffer.prototype.readIntBE = function readIntBE (offset, byteLength, noAssert) {
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) checkOffset(offset, byteLength, this.length)

  var i = byteLength
  var mul = 1
  var val = this[offset + --i]
  while (i > 0 && (mul *= 0x100)) {
    val += this[offset + --i] * mul
  }
  mul *= 0x80

  if (val >= mul) val -= Math.pow(2, 8 * byteLength)

  return val
}

Buffer.prototype.readInt8 = function readInt8 (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 1, this.length)
  if (!(this[offset] & 0x80)) return (this[offset])
  return ((0xff - this[offset] + 1) * -1)
}

Buffer.prototype.readInt16LE = function readInt16LE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 2, this.length)
  var val = this[offset] | (this[offset + 1] << 8)
  return (val & 0x8000) ? val | 0xFFFF0000 : val
}

Buffer.prototype.readInt16BE = function readInt16BE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 2, this.length)
  var val = this[offset + 1] | (this[offset] << 8)
  return (val & 0x8000) ? val | 0xFFFF0000 : val
}

Buffer.prototype.readInt32LE = function readInt32LE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset]) |
    (this[offset + 1] << 8) |
    (this[offset + 2] << 16) |
    (this[offset + 3] << 24)
}

Buffer.prototype.readInt32BE = function readInt32BE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)

  return (this[offset] << 24) |
    (this[offset + 1] << 16) |
    (this[offset + 2] << 8) |
    (this[offset + 3])
}

Buffer.prototype.readFloatLE = function readFloatLE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)
  return ieee754.read(this, offset, true, 23, 4)
}

Buffer.prototype.readFloatBE = function readFloatBE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 4, this.length)
  return ieee754.read(this, offset, false, 23, 4)
}

Buffer.prototype.readDoubleLE = function readDoubleLE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 8, this.length)
  return ieee754.read(this, offset, true, 52, 8)
}

Buffer.prototype.readDoubleBE = function readDoubleBE (offset, noAssert) {
  offset = offset >>> 0
  if (!noAssert) checkOffset(offset, 8, this.length)
  return ieee754.read(this, offset, false, 52, 8)
}

function checkInt (buf, value, offset, ext, max, min) {
  if (!Buffer.isBuffer(buf)) throw new TypeError('"buffer" argument must be a Buffer instance')
  if (value > max || value < min) throw new RangeError('"value" argument is out of bounds')
  if (offset + ext > buf.length) throw new RangeError('Index out of range')
}

Buffer.prototype.writeUIntLE = function writeUIntLE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) {
    var maxBytes = Math.pow(2, 8 * byteLength) - 1
    checkInt(this, value, offset, byteLength, maxBytes, 0)
  }

  var mul = 1
  var i = 0
  this[offset] = value & 0xFF
  while (++i < byteLength && (mul *= 0x100)) {
    this[offset + i] = (value / mul) & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeUIntBE = function writeUIntBE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset >>> 0
  byteLength = byteLength >>> 0
  if (!noAssert) {
    var maxBytes = Math.pow(2, 8 * byteLength) - 1
    checkInt(this, value, offset, byteLength, maxBytes, 0)
  }

  var i = byteLength - 1
  var mul = 1
  this[offset + i] = value & 0xFF
  while (--i >= 0 && (mul *= 0x100)) {
    this[offset + i] = (value / mul) & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeUInt8 = function writeUInt8 (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 1, 0xff, 0)
  this[offset] = (value & 0xff)
  return offset + 1
}

Buffer.prototype.writeUInt16LE = function writeUInt16LE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0)
  this[offset] = (value & 0xff)
  this[offset + 1] = (value >>> 8)
  return offset + 2
}

Buffer.prototype.writeUInt16BE = function writeUInt16BE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0)
  this[offset] = (value >>> 8)
  this[offset + 1] = (value & 0xff)
  return offset + 2
}

Buffer.prototype.writeUInt32LE = function writeUInt32LE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0)
  this[offset + 3] = (value >>> 24)
  this[offset + 2] = (value >>> 16)
  this[offset + 1] = (value >>> 8)
  this[offset] = (value & 0xff)
  return offset + 4
}

Buffer.prototype.writeUInt32BE = function writeUInt32BE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0)
  this[offset] = (value >>> 24)
  this[offset + 1] = (value >>> 16)
  this[offset + 2] = (value >>> 8)
  this[offset + 3] = (value & 0xff)
  return offset + 4
}

Buffer.prototype.writeIntLE = function writeIntLE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) {
    var limit = Math.pow(2, (8 * byteLength) - 1)

    checkInt(this, value, offset, byteLength, limit - 1, -limit)
  }

  var i = 0
  var mul = 1
  var sub = 0
  this[offset] = value & 0xFF
  while (++i < byteLength && (mul *= 0x100)) {
    if (value < 0 && sub === 0 && this[offset + i - 1] !== 0) {
      sub = 1
    }
    this[offset + i] = ((value / mul) >> 0) - sub & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeIntBE = function writeIntBE (value, offset, byteLength, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) {
    var limit = Math.pow(2, (8 * byteLength) - 1)

    checkInt(this, value, offset, byteLength, limit - 1, -limit)
  }

  var i = byteLength - 1
  var mul = 1
  var sub = 0
  this[offset + i] = value & 0xFF
  while (--i >= 0 && (mul *= 0x100)) {
    if (value < 0 && sub === 0 && this[offset + i + 1] !== 0) {
      sub = 1
    }
    this[offset + i] = ((value / mul) >> 0) - sub & 0xFF
  }

  return offset + byteLength
}

Buffer.prototype.writeInt8 = function writeInt8 (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 1, 0x7f, -0x80)
  if (value < 0) value = 0xff + value + 1
  this[offset] = (value & 0xff)
  return offset + 1
}

Buffer.prototype.writeInt16LE = function writeInt16LE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000)
  this[offset] = (value & 0xff)
  this[offset + 1] = (value >>> 8)
  return offset + 2
}

Buffer.prototype.writeInt16BE = function writeInt16BE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000)
  this[offset] = (value >>> 8)
  this[offset + 1] = (value & 0xff)
  return offset + 2
}

Buffer.prototype.writeInt32LE = function writeInt32LE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000)
  this[offset] = (value & 0xff)
  this[offset + 1] = (value >>> 8)
  this[offset + 2] = (value >>> 16)
  this[offset + 3] = (value >>> 24)
  return offset + 4
}

Buffer.prototype.writeInt32BE = function writeInt32BE (value, offset, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000)
  if (value < 0) value = 0xffffffff + value + 1
  this[offset] = (value >>> 24)
  this[offset + 1] = (value >>> 16)
  this[offset + 2] = (value >>> 8)
  this[offset + 3] = (value & 0xff)
  return offset + 4
}

function checkIEEE754 (buf, value, offset, ext, max, min) {
  if (offset + ext > buf.length) throw new RangeError('Index out of range')
  if (offset < 0) throw new RangeError('Index out of range')
}

function writeFloat (buf, value, offset, littleEndian, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 4, 3.4028234663852886e+38, -3.4028234663852886e+38)
  }
  ieee754.write(buf, value, offset, littleEndian, 23, 4)
  return offset + 4
}

Buffer.prototype.writeFloatLE = function writeFloatLE (value, offset, noAssert) {
  return writeFloat(this, value, offset, true, noAssert)
}

Buffer.prototype.writeFloatBE = function writeFloatBE (value, offset, noAssert) {
  return writeFloat(this, value, offset, false, noAssert)
}

function writeDouble (buf, value, offset, littleEndian, noAssert) {
  value = +value
  offset = offset >>> 0
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 8, 1.7976931348623157E+308, -1.7976931348623157E+308)
  }
  ieee754.write(buf, value, offset, littleEndian, 52, 8)
  return offset + 8
}

Buffer.prototype.writeDoubleLE = function writeDoubleLE (value, offset, noAssert) {
  return writeDouble(this, value, offset, true, noAssert)
}

Buffer.prototype.writeDoubleBE = function writeDoubleBE (value, offset, noAssert) {
  return writeDouble(this, value, offset, false, noAssert)
}

// copy(targetBuffer, targetStart=0, sourceStart=0, sourceEnd=buffer.length)
Buffer.prototype.copy = function copy (target, targetStart, start, end) {
  if (!Buffer.isBuffer(target)) throw new TypeError('argument should be a Buffer')
  if (!start) start = 0
  if (!end && end !== 0) end = this.length
  if (targetStart >= target.length) targetStart = target.length
  if (!targetStart) targetStart = 0
  if (end > 0 && end < start) end = start

  // Copy 0 bytes; we're done
  if (end === start) return 0
  if (target.length === 0 || this.length === 0) return 0

  // Fatal error conditions
  if (targetStart < 0) {
    throw new RangeError('targetStart out of bounds')
  }
  if (start < 0 || start >= this.length) throw new RangeError('Index out of range')
  if (end < 0) throw new RangeError('sourceEnd out of bounds')

  // Are we oob?
  if (end > this.length) end = this.length
  if (target.length - targetStart < end - start) {
    end = target.length - targetStart + start
  }

  var len = end - start

  if (this === target && typeof Uint8Array.prototype.copyWithin === 'function') {
    // Use built-in when available, missing from IE11
    this.copyWithin(targetStart, start, end)
  } else if (this === target && start < targetStart && targetStart < end) {
    // descending copy from end
    for (var i = len - 1; i >= 0; --i) {
      target[i + targetStart] = this[i + start]
    }
  } else {
    Uint8Array.prototype.set.call(
      target,
      this.subarray(start, end),
      targetStart
    )
  }

  return len
}

// Usage:
//    buffer.fill(number[, offset[, end]])
//    buffer.fill(buffer[, offset[, end]])
//    buffer.fill(string[, offset[, end]][, encoding])
Buffer.prototype.fill = function fill (val, start, end, encoding) {
  // Handle string cases:
  if (typeof val === 'string') {
    if (typeof start === 'string') {
      encoding = start
      start = 0
      end = this.length
    } else if (typeof end === 'string') {
      encoding = end
      end = this.length
    }
    if (encoding !== undefined && typeof encoding !== 'string') {
      throw new TypeError('encoding must be a string')
    }
    if (typeof encoding === 'string' && !Buffer.isEncoding(encoding)) {
      throw new TypeError('Unknown encoding: ' + encoding)
    }
    if (val.length === 1) {
      var code = val.charCodeAt(0)
      if ((encoding === 'utf8' && code < 128) ||
          encoding === 'latin1') {
        // Fast path: If `val` fits into a single byte, use that numeric value.
        val = code
      }
    }
  } else if (typeof val === 'number') {
    val = val & 255
  }

  // Invalid ranges are not set to a default, so can range check early.
  if (start < 0 || this.length < start || this.length < end) {
    throw new RangeError('Out of range index')
  }

  if (end <= start) {
    return this
  }

  start = start >>> 0
  end = end === undefined ? this.length : end >>> 0

  if (!val) val = 0

  var i
  if (typeof val === 'number') {
    for (i = start; i < end; ++i) {
      this[i] = val
    }
  } else {
    var bytes = Buffer.isBuffer(val)
      ? val
      : Buffer.from(val, encoding)
    var len = bytes.length
    if (len === 0) {
      throw new TypeError('The value "' + val +
        '" is invalid for argument "value"')
    }
    for (i = 0; i < end - start; ++i) {
      this[i + start] = bytes[i % len]
    }
  }

  return this
}

// HELPER FUNCTIONS
// ================

var INVALID_BASE64_RE = /[^+/0-9A-Za-z-_]/g

function base64clean (str) {
  // Node takes equal signs as end of the Base64 encoding
  str = str.split('=')[0]
  // Node strips out invalid characters like \n and \t from the string, base64-js does not
  str = str.trim().replace(INVALID_BASE64_RE, '')
  // Node converts strings with length < 2 to ''
  if (str.length < 2) return ''
  // Node allows for non-padded base64 strings (missing trailing ===), base64-js does not
  while (str.length % 4 !== 0) {
    str = str + '='
  }
  return str
}

function toHex (n) {
  if (n < 16) return '0' + n.toString(16)
  return n.toString(16)
}

function utf8ToBytes (string, units) {
  units = units || Infinity
  var codePoint
  var length = string.length
  var leadSurrogate = null
  var bytes = []

  for (var i = 0; i < length; ++i) {
    codePoint = string.charCodeAt(i)

    // is surrogate component
    if (codePoint > 0xD7FF && codePoint < 0xE000) {
      // last char was a lead
      if (!leadSurrogate) {
        // no lead yet
        if (codePoint > 0xDBFF) {
          // unexpected trail
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
          continue
        } else if (i + 1 === length) {
          // unpaired lead
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
          continue
        }

        // valid lead
        leadSurrogate = codePoint

        continue
      }

      // 2 leads in a row
      if (codePoint < 0xDC00) {
        if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
        leadSurrogate = codePoint
        continue
      }

      // valid surrogate pair
      codePoint = (leadSurrogate - 0xD800 << 10 | codePoint - 0xDC00) + 0x10000
    } else if (leadSurrogate) {
      // valid bmp char, but last char was a lead
      if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD)
    }

    leadSurrogate = null

    // encode utf8
    if (codePoint < 0x80) {
      if ((units -= 1) < 0) break
      bytes.push(codePoint)
    } else if (codePoint < 0x800) {
      if ((units -= 2) < 0) break
      bytes.push(
        codePoint >> 0x6 | 0xC0,
        codePoint & 0x3F | 0x80
      )
    } else if (codePoint < 0x10000) {
      if ((units -= 3) < 0) break
      bytes.push(
        codePoint >> 0xC | 0xE0,
        codePoint >> 0x6 & 0x3F | 0x80,
        codePoint & 0x3F | 0x80
      )
    } else if (codePoint < 0x110000) {
      if ((units -= 4) < 0) break
      bytes.push(
        codePoint >> 0x12 | 0xF0,
        codePoint >> 0xC & 0x3F | 0x80,
        codePoint >> 0x6 & 0x3F | 0x80,
        codePoint & 0x3F | 0x80
      )
    } else {
      throw new Error('Invalid code point')
    }
  }

  return bytes
}

function asciiToBytes (str) {
  var byteArray = []
  for (var i = 0; i < str.length; ++i) {
    // Node's code seems to be doing this and not & 0x7F..
    byteArray.push(str.charCodeAt(i) & 0xFF)
  }
  return byteArray
}

function utf16leToBytes (str, units) {
  var c, hi, lo
  var byteArray = []
  for (var i = 0; i < str.length; ++i) {
    if ((units -= 2) < 0) break

    c = str.charCodeAt(i)
    hi = c >> 8
    lo = c % 256
    byteArray.push(lo)
    byteArray.push(hi)
  }

  return byteArray
}

function base64ToBytes (str) {
  return base64.toByteArray(base64clean(str))
}

function blitBuffer (src, dst, offset, length) {
  for (var i = 0; i < length; ++i) {
    if ((i + offset >= dst.length) || (i >= src.length)) break
    dst[i + offset] = src[i]
  }
  return i
}

// ArrayBuffer or Uint8Array objects from other contexts (i.e. iframes) do not pass
// the `instanceof` check but they should be treated as of that type.
// See: https://github.com/feross/buffer/issues/166
function isInstance (obj, type) {
  return obj instanceof type ||
    (obj != null && obj.constructor != null && obj.constructor.name != null &&
      obj.constructor.name === type.name)
}
function numberIsNaN (obj) {
  // For IE11 support
  return obj !== obj // eslint-disable-line no-self-compare
}

}).call(this,require("buffer").Buffer)
},{"base64-js":35,"buffer":37,"ieee754":40}],38:[function(require,module,exports){
(function (Buffer){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.

function isArray(arg) {
  if (Array.isArray) {
    return Array.isArray(arg);
  }
  return objectToString(arg) === '[object Array]';
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

exports.isBuffer = Buffer.isBuffer;

function objectToString(o) {
  return Object.prototype.toString.call(o);
}

}).call(this,{"isBuffer":require("../../is-buffer/index.js")})
},{"../../is-buffer/index.js":42}],39:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var objectCreate = Object.create || objectCreatePolyfill
var objectKeys = Object.keys || objectKeysPolyfill
var bind = Function.prototype.bind || functionBindPolyfill

function EventEmitter() {
  if (!this._events || !Object.prototype.hasOwnProperty.call(this, '_events')) {
    this._events = objectCreate(null);
    this._eventsCount = 0;
  }

  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
var defaultMaxListeners = 10;

var hasDefineProperty;
try {
  var o = {};
  if (Object.defineProperty) Object.defineProperty(o, 'x', { value: 0 });
  hasDefineProperty = o.x === 0;
} catch (err) { hasDefineProperty = false }
if (hasDefineProperty) {
  Object.defineProperty(EventEmitter, 'defaultMaxListeners', {
    enumerable: true,
    get: function() {
      return defaultMaxListeners;
    },
    set: function(arg) {
      // check whether the input is a positive number (whose value is zero or
      // greater and not a NaN).
      if (typeof arg !== 'number' || arg < 0 || arg !== arg)
        throw new TypeError('"defaultMaxListeners" must be a positive number');
      defaultMaxListeners = arg;
    }
  });
} else {
  EventEmitter.defaultMaxListeners = defaultMaxListeners;
}

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function setMaxListeners(n) {
  if (typeof n !== 'number' || n < 0 || isNaN(n))
    throw new TypeError('"n" argument must be a positive number');
  this._maxListeners = n;
  return this;
};

function $getMaxListeners(that) {
  if (that._maxListeners === undefined)
    return EventEmitter.defaultMaxListeners;
  return that._maxListeners;
}

EventEmitter.prototype.getMaxListeners = function getMaxListeners() {
  return $getMaxListeners(this);
};

// These standalone emit* functions are used to optimize calling of event
// handlers for fast cases because emit() itself often has a variable number of
// arguments and can be deoptimized because of that. These functions always have
// the same number of arguments and thus do not get deoptimized, so the code
// inside them can execute faster.
function emitNone(handler, isFn, self) {
  if (isFn)
    handler.call(self);
  else {
    var len = handler.length;
    var listeners = arrayClone(handler, len);
    for (var i = 0; i < len; ++i)
      listeners[i].call(self);
  }
}
function emitOne(handler, isFn, self, arg1) {
  if (isFn)
    handler.call(self, arg1);
  else {
    var len = handler.length;
    var listeners = arrayClone(handler, len);
    for (var i = 0; i < len; ++i)
      listeners[i].call(self, arg1);
  }
}
function emitTwo(handler, isFn, self, arg1, arg2) {
  if (isFn)
    handler.call(self, arg1, arg2);
  else {
    var len = handler.length;
    var listeners = arrayClone(handler, len);
    for (var i = 0; i < len; ++i)
      listeners[i].call(self, arg1, arg2);
  }
}
function emitThree(handler, isFn, self, arg1, arg2, arg3) {
  if (isFn)
    handler.call(self, arg1, arg2, arg3);
  else {
    var len = handler.length;
    var listeners = arrayClone(handler, len);
    for (var i = 0; i < len; ++i)
      listeners[i].call(self, arg1, arg2, arg3);
  }
}

function emitMany(handler, isFn, self, args) {
  if (isFn)
    handler.apply(self, args);
  else {
    var len = handler.length;
    var listeners = arrayClone(handler, len);
    for (var i = 0; i < len; ++i)
      listeners[i].apply(self, args);
  }
}

EventEmitter.prototype.emit = function emit(type) {
  var er, handler, len, args, i, events;
  var doError = (type === 'error');

  events = this._events;
  if (events)
    doError = (doError && events.error == null);
  else if (!doError)
    return false;

  // If there is no 'error' event listener then throw.
  if (doError) {
    if (arguments.length > 1)
      er = arguments[1];
    if (er instanceof Error) {
      throw er; // Unhandled 'error' event
    } else {
      // At least give some kind of context to the user
      var err = new Error('Unhandled "error" event. (' + er + ')');
      err.context = er;
      throw err;
    }
    return false;
  }

  handler = events[type];

  if (!handler)
    return false;

  var isFn = typeof handler === 'function';
  len = arguments.length;
  switch (len) {
      // fast cases
    case 1:
      emitNone(handler, isFn, this);
      break;
    case 2:
      emitOne(handler, isFn, this, arguments[1]);
      break;
    case 3:
      emitTwo(handler, isFn, this, arguments[1], arguments[2]);
      break;
    case 4:
      emitThree(handler, isFn, this, arguments[1], arguments[2], arguments[3]);
      break;
      // slower
    default:
      args = new Array(len - 1);
      for (i = 1; i < len; i++)
        args[i - 1] = arguments[i];
      emitMany(handler, isFn, this, args);
  }

  return true;
};

function _addListener(target, type, listener, prepend) {
  var m;
  var events;
  var existing;

  if (typeof listener !== 'function')
    throw new TypeError('"listener" argument must be a function');

  events = target._events;
  if (!events) {
    events = target._events = objectCreate(null);
    target._eventsCount = 0;
  } else {
    // To avoid recursion in the case that type === "newListener"! Before
    // adding it to the listeners, first emit "newListener".
    if (events.newListener) {
      target.emit('newListener', type,
          listener.listener ? listener.listener : listener);

      // Re-assign `events` because a newListener handler could have caused the
      // this._events to be assigned to a new object
      events = target._events;
    }
    existing = events[type];
  }

  if (!existing) {
    // Optimize the case of one listener. Don't need the extra array object.
    existing = events[type] = listener;
    ++target._eventsCount;
  } else {
    if (typeof existing === 'function') {
      // Adding the second element, need to change to array.
      existing = events[type] =
          prepend ? [listener, existing] : [existing, listener];
    } else {
      // If we've already got an array, just append.
      if (prepend) {
        existing.unshift(listener);
      } else {
        existing.push(listener);
      }
    }

    // Check for listener leak
    if (!existing.warned) {
      m = $getMaxListeners(target);
      if (m && m > 0 && existing.length > m) {
        existing.warned = true;
        var w = new Error('Possible EventEmitter memory leak detected. ' +
            existing.length + ' "' + String(type) + '" listeners ' +
            'added. Use emitter.setMaxListeners() to ' +
            'increase limit.');
        w.name = 'MaxListenersExceededWarning';
        w.emitter = target;
        w.type = type;
        w.count = existing.length;
        if (typeof console === 'object' && console.warn) {
          console.warn('%s: %s', w.name, w.message);
        }
      }
    }
  }

  return target;
}

EventEmitter.prototype.addListener = function addListener(type, listener) {
  return _addListener(this, type, listener, false);
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.prependListener =
    function prependListener(type, listener) {
      return _addListener(this, type, listener, true);
    };

function onceWrapper() {
  if (!this.fired) {
    this.target.removeListener(this.type, this.wrapFn);
    this.fired = true;
    switch (arguments.length) {
      case 0:
        return this.listener.call(this.target);
      case 1:
        return this.listener.call(this.target, arguments[0]);
      case 2:
        return this.listener.call(this.target, arguments[0], arguments[1]);
      case 3:
        return this.listener.call(this.target, arguments[0], arguments[1],
            arguments[2]);
      default:
        var args = new Array(arguments.length);
        for (var i = 0; i < args.length; ++i)
          args[i] = arguments[i];
        this.listener.apply(this.target, args);
    }
  }
}

function _onceWrap(target, type, listener) {
  var state = { fired: false, wrapFn: undefined, target: target, type: type, listener: listener };
  var wrapped = bind.call(onceWrapper, state);
  wrapped.listener = listener;
  state.wrapFn = wrapped;
  return wrapped;
}

EventEmitter.prototype.once = function once(type, listener) {
  if (typeof listener !== 'function')
    throw new TypeError('"listener" argument must be a function');
  this.on(type, _onceWrap(this, type, listener));
  return this;
};

EventEmitter.prototype.prependOnceListener =
    function prependOnceListener(type, listener) {
      if (typeof listener !== 'function')
        throw new TypeError('"listener" argument must be a function');
      this.prependListener(type, _onceWrap(this, type, listener));
      return this;
    };

// Emits a 'removeListener' event if and only if the listener was removed.
EventEmitter.prototype.removeListener =
    function removeListener(type, listener) {
      var list, events, position, i, originalListener;

      if (typeof listener !== 'function')
        throw new TypeError('"listener" argument must be a function');

      events = this._events;
      if (!events)
        return this;

      list = events[type];
      if (!list)
        return this;

      if (list === listener || list.listener === listener) {
        if (--this._eventsCount === 0)
          this._events = objectCreate(null);
        else {
          delete events[type];
          if (events.removeListener)
            this.emit('removeListener', type, list.listener || listener);
        }
      } else if (typeof list !== 'function') {
        position = -1;

        for (i = list.length - 1; i >= 0; i--) {
          if (list[i] === listener || list[i].listener === listener) {
            originalListener = list[i].listener;
            position = i;
            break;
          }
        }

        if (position < 0)
          return this;

        if (position === 0)
          list.shift();
        else
          spliceOne(list, position);

        if (list.length === 1)
          events[type] = list[0];

        if (events.removeListener)
          this.emit('removeListener', type, originalListener || listener);
      }

      return this;
    };

EventEmitter.prototype.removeAllListeners =
    function removeAllListeners(type) {
      var listeners, events, i;

      events = this._events;
      if (!events)
        return this;

      // not listening for removeListener, no need to emit
      if (!events.removeListener) {
        if (arguments.length === 0) {
          this._events = objectCreate(null);
          this._eventsCount = 0;
        } else if (events[type]) {
          if (--this._eventsCount === 0)
            this._events = objectCreate(null);
          else
            delete events[type];
        }
        return this;
      }

      // emit removeListener for all listeners on all events
      if (arguments.length === 0) {
        var keys = objectKeys(events);
        var key;
        for (i = 0; i < keys.length; ++i) {
          key = keys[i];
          if (key === 'removeListener') continue;
          this.removeAllListeners(key);
        }
        this.removeAllListeners('removeListener');
        this._events = objectCreate(null);
        this._eventsCount = 0;
        return this;
      }

      listeners = events[type];

      if (typeof listeners === 'function') {
        this.removeListener(type, listeners);
      } else if (listeners) {
        // LIFO order
        for (i = listeners.length - 1; i >= 0; i--) {
          this.removeListener(type, listeners[i]);
        }
      }

      return this;
    };

function _listeners(target, type, unwrap) {
  var events = target._events;

  if (!events)
    return [];

  var evlistener = events[type];
  if (!evlistener)
    return [];

  if (typeof evlistener === 'function')
    return unwrap ? [evlistener.listener || evlistener] : [evlistener];

  return unwrap ? unwrapListeners(evlistener) : arrayClone(evlistener, evlistener.length);
}

EventEmitter.prototype.listeners = function listeners(type) {
  return _listeners(this, type, true);
};

EventEmitter.prototype.rawListeners = function rawListeners(type) {
  return _listeners(this, type, false);
};

EventEmitter.listenerCount = function(emitter, type) {
  if (typeof emitter.listenerCount === 'function') {
    return emitter.listenerCount(type);
  } else {
    return listenerCount.call(emitter, type);
  }
};

EventEmitter.prototype.listenerCount = listenerCount;
function listenerCount(type) {
  var events = this._events;

  if (events) {
    var evlistener = events[type];

    if (typeof evlistener === 'function') {
      return 1;
    } else if (evlistener) {
      return evlistener.length;
    }
  }

  return 0;
}

EventEmitter.prototype.eventNames = function eventNames() {
  return this._eventsCount > 0 ? Reflect.ownKeys(this._events) : [];
};

// About 1.5x faster than the two-arg version of Array#splice().
function spliceOne(list, index) {
  for (var i = index, k = i + 1, n = list.length; k < n; i += 1, k += 1)
    list[i] = list[k];
  list.pop();
}

function arrayClone(arr, n) {
  var copy = new Array(n);
  for (var i = 0; i < n; ++i)
    copy[i] = arr[i];
  return copy;
}

function unwrapListeners(arr) {
  var ret = new Array(arr.length);
  for (var i = 0; i < ret.length; ++i) {
    ret[i] = arr[i].listener || arr[i];
  }
  return ret;
}

function objectCreatePolyfill(proto) {
  var F = function() {};
  F.prototype = proto;
  return new F;
}
function objectKeysPolyfill(obj) {
  var keys = [];
  for (var k in obj) if (Object.prototype.hasOwnProperty.call(obj, k)) {
    keys.push(k);
  }
  return k;
}
function functionBindPolyfill(context) {
  var fn = this;
  return function () {
    return fn.apply(context, arguments);
  };
}

},{}],40:[function(require,module,exports){
exports.read = function (buffer, offset, isLE, mLen, nBytes) {
  var e, m
  var eLen = (nBytes * 8) - mLen - 1
  var eMax = (1 << eLen) - 1
  var eBias = eMax >> 1
  var nBits = -7
  var i = isLE ? (nBytes - 1) : 0
  var d = isLE ? -1 : 1
  var s = buffer[offset + i]

  i += d

  e = s & ((1 << (-nBits)) - 1)
  s >>= (-nBits)
  nBits += eLen
  for (; nBits > 0; e = (e * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  m = e & ((1 << (-nBits)) - 1)
  e >>= (-nBits)
  nBits += mLen
  for (; nBits > 0; m = (m * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  if (e === 0) {
    e = 1 - eBias
  } else if (e === eMax) {
    return m ? NaN : ((s ? -1 : 1) * Infinity)
  } else {
    m = m + Math.pow(2, mLen)
    e = e - eBias
  }
  return (s ? -1 : 1) * m * Math.pow(2, e - mLen)
}

exports.write = function (buffer, value, offset, isLE, mLen, nBytes) {
  var e, m, c
  var eLen = (nBytes * 8) - mLen - 1
  var eMax = (1 << eLen) - 1
  var eBias = eMax >> 1
  var rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0)
  var i = isLE ? 0 : (nBytes - 1)
  var d = isLE ? 1 : -1
  var s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0

  value = Math.abs(value)

  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? 1 : 0
    e = eMax
  } else {
    e = Math.floor(Math.log(value) / Math.LN2)
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--
      c *= 2
    }
    if (e + eBias >= 1) {
      value += rt / c
    } else {
      value += rt * Math.pow(2, 1 - eBias)
    }
    if (value * c >= 2) {
      e++
      c /= 2
    }

    if (e + eBias >= eMax) {
      m = 0
      e = eMax
    } else if (e + eBias >= 1) {
      m = ((value * c) - 1) * Math.pow(2, mLen)
      e = e + eBias
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen)
      e = 0
    }
  }

  for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

  e = (e << mLen) | m
  eLen += mLen
  for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

  buffer[offset + i - d] |= s * 128
}

},{}],41:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    if (superCtor) {
      ctor.super_ = superCtor
      ctor.prototype = Object.create(superCtor.prototype, {
        constructor: {
          value: ctor,
          enumerable: false,
          writable: true,
          configurable: true
        }
      })
    }
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    if (superCtor) {
      ctor.super_ = superCtor
      var TempCtor = function () {}
      TempCtor.prototype = superCtor.prototype
      ctor.prototype = new TempCtor()
      ctor.prototype.constructor = ctor
    }
  }
}

},{}],42:[function(require,module,exports){
/*!
 * Determine if an object is a Buffer
 *
 * @author   Feross Aboukhadijeh <https://feross.org>
 * @license  MIT
 */

// The _isBuffer check is for Safari 5-7 support, because it's missing
// Object.prototype.constructor. Remove this eventually
module.exports = function (obj) {
  return obj != null && (isBuffer(obj) || isSlowBuffer(obj) || !!obj._isBuffer)
}

function isBuffer (obj) {
  return !!obj.constructor && typeof obj.constructor.isBuffer === 'function' && obj.constructor.isBuffer(obj)
}

// For Node v0.10 support. Remove this eventually.
function isSlowBuffer (obj) {
  return typeof obj.readFloatLE === 'function' && typeof obj.slice === 'function' && isBuffer(obj.slice(0, 0))
}

},{}],43:[function(require,module,exports){
var toString = {}.toString;

module.exports = Array.isArray || function (arr) {
  return toString.call(arr) == '[object Array]';
};

},{}],44:[function(require,module,exports){
(function (process){
// .dirname, .basename, and .extname methods are extracted from Node.js v8.11.1,
// backported and transplited with Babel, with backwards-compat fixes

// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// resolves . and .. elements in a path array with directory names there
// must be no slashes, empty elements, or device names (c:\) in the array
// (so also no leading and trailing slashes - it does not distinguish
// relative and absolute paths)
function normalizeArray(parts, allowAboveRoot) {
  // if the path tries to go above the root, `up` ends up > 0
  var up = 0;
  for (var i = parts.length - 1; i >= 0; i--) {
    var last = parts[i];
    if (last === '.') {
      parts.splice(i, 1);
    } else if (last === '..') {
      parts.splice(i, 1);
      up++;
    } else if (up) {
      parts.splice(i, 1);
      up--;
    }
  }

  // if the path is allowed to go above the root, restore leading ..s
  if (allowAboveRoot) {
    for (; up--; up) {
      parts.unshift('..');
    }
  }

  return parts;
}

// path.resolve([from ...], to)
// posix version
exports.resolve = function() {
  var resolvedPath = '',
      resolvedAbsolute = false;

  for (var i = arguments.length - 1; i >= -1 && !resolvedAbsolute; i--) {
    var path = (i >= 0) ? arguments[i] : process.cwd();

    // Skip empty and invalid entries
    if (typeof path !== 'string') {
      throw new TypeError('Arguments to path.resolve must be strings');
    } else if (!path) {
      continue;
    }

    resolvedPath = path + '/' + resolvedPath;
    resolvedAbsolute = path.charAt(0) === '/';
  }

  // At this point the path should be resolved to a full absolute path, but
  // handle relative paths to be safe (might happen when process.cwd() fails)

  // Normalize the path
  resolvedPath = normalizeArray(filter(resolvedPath.split('/'), function(p) {
    return !!p;
  }), !resolvedAbsolute).join('/');

  return ((resolvedAbsolute ? '/' : '') + resolvedPath) || '.';
};

// path.normalize(path)
// posix version
exports.normalize = function(path) {
  var isAbsolute = exports.isAbsolute(path),
      trailingSlash = substr(path, -1) === '/';

  // Normalize the path
  path = normalizeArray(filter(path.split('/'), function(p) {
    return !!p;
  }), !isAbsolute).join('/');

  if (!path && !isAbsolute) {
    path = '.';
  }
  if (path && trailingSlash) {
    path += '/';
  }

  return (isAbsolute ? '/' : '') + path;
};

// posix version
exports.isAbsolute = function(path) {
  return path.charAt(0) === '/';
};

// posix version
exports.join = function() {
  var paths = Array.prototype.slice.call(arguments, 0);
  return exports.normalize(filter(paths, function(p, index) {
    if (typeof p !== 'string') {
      throw new TypeError('Arguments to path.join must be strings');
    }
    return p;
  }).join('/'));
};


// path.relative(from, to)
// posix version
exports.relative = function(from, to) {
  from = exports.resolve(from).substr(1);
  to = exports.resolve(to).substr(1);

  function trim(arr) {
    var start = 0;
    for (; start < arr.length; start++) {
      if (arr[start] !== '') break;
    }

    var end = arr.length - 1;
    for (; end >= 0; end--) {
      if (arr[end] !== '') break;
    }

    if (start > end) return [];
    return arr.slice(start, end - start + 1);
  }

  var fromParts = trim(from.split('/'));
  var toParts = trim(to.split('/'));

  var length = Math.min(fromParts.length, toParts.length);
  var samePartsLength = length;
  for (var i = 0; i < length; i++) {
    if (fromParts[i] !== toParts[i]) {
      samePartsLength = i;
      break;
    }
  }

  var outputParts = [];
  for (var i = samePartsLength; i < fromParts.length; i++) {
    outputParts.push('..');
  }

  outputParts = outputParts.concat(toParts.slice(samePartsLength));

  return outputParts.join('/');
};

exports.sep = '/';
exports.delimiter = ':';

exports.dirname = function (path) {
  if (typeof path !== 'string') path = path + '';
  if (path.length === 0) return '.';
  var code = path.charCodeAt(0);
  var hasRoot = code === 47 /*/*/;
  var end = -1;
  var matchedSlash = true;
  for (var i = path.length - 1; i >= 1; --i) {
    code = path.charCodeAt(i);
    if (code === 47 /*/*/) {
        if (!matchedSlash) {
          end = i;
          break;
        }
      } else {
      // We saw the first non-path separator
      matchedSlash = false;
    }
  }

  if (end === -1) return hasRoot ? '/' : '.';
  if (hasRoot && end === 1) {
    // return '//';
    // Backwards-compat fix:
    return '/';
  }
  return path.slice(0, end);
};

function basename(path) {
  if (typeof path !== 'string') path = path + '';

  var start = 0;
  var end = -1;
  var matchedSlash = true;
  var i;

  for (i = path.length - 1; i >= 0; --i) {
    if (path.charCodeAt(i) === 47 /*/*/) {
        // If we reached a path separator that was not part of a set of path
        // separators at the end of the string, stop now
        if (!matchedSlash) {
          start = i + 1;
          break;
        }
      } else if (end === -1) {
      // We saw the first non-path separator, mark this as the end of our
      // path component
      matchedSlash = false;
      end = i + 1;
    }
  }

  if (end === -1) return '';
  return path.slice(start, end);
}

// Uses a mixed approach for backwards-compatibility, as ext behavior changed
// in new Node.js versions, so only basename() above is backported here
exports.basename = function (path, ext) {
  var f = basename(path);
  if (ext && f.substr(-1 * ext.length) === ext) {
    f = f.substr(0, f.length - ext.length);
  }
  return f;
};

exports.extname = function (path) {
  if (typeof path !== 'string') path = path + '';
  var startDot = -1;
  var startPart = 0;
  var end = -1;
  var matchedSlash = true;
  // Track the state of characters (if any) we see before our first dot and
  // after any path separator we find
  var preDotState = 0;
  for (var i = path.length - 1; i >= 0; --i) {
    var code = path.charCodeAt(i);
    if (code === 47 /*/*/) {
        // If we reached a path separator that was not part of a set of path
        // separators at the end of the string, stop now
        if (!matchedSlash) {
          startPart = i + 1;
          break;
        }
        continue;
      }
    if (end === -1) {
      // We saw the first non-path separator, mark this as the end of our
      // extension
      matchedSlash = false;
      end = i + 1;
    }
    if (code === 46 /*.*/) {
        // If this is our first dot, mark it as the start of our extension
        if (startDot === -1)
          startDot = i;
        else if (preDotState !== 1)
          preDotState = 1;
    } else if (startDot !== -1) {
      // We saw a non-dot and non-path separator before our dot, so we should
      // have a good chance at having a non-empty extension
      preDotState = -1;
    }
  }

  if (startDot === -1 || end === -1 ||
      // We saw a non-dot character immediately before the dot
      preDotState === 0 ||
      // The (right-most) trimmed path component is exactly '..'
      preDotState === 1 && startDot === end - 1 && startDot === startPart + 1) {
    return '';
  }
  return path.slice(startDot, end);
};

function filter (xs, f) {
    if (xs.filter) return xs.filter(f);
    var res = [];
    for (var i = 0; i < xs.length; i++) {
        if (f(xs[i], i, xs)) res.push(xs[i]);
    }
    return res;
}

// String.prototype.substr - negative index don't work in IE8
var substr = 'ab'.substr(-1) === 'b'
    ? function (str, start, len) { return str.substr(start, len) }
    : function (str, start, len) {
        if (start < 0) start = str.length + start;
        return str.substr(start, len);
    }
;

}).call(this,require('_process'))
},{"_process":46}],45:[function(require,module,exports){
(function (process){
'use strict';

if (typeof process === 'undefined' ||
    !process.version ||
    process.version.indexOf('v0.') === 0 ||
    process.version.indexOf('v1.') === 0 && process.version.indexOf('v1.8.') !== 0) {
  module.exports = { nextTick: nextTick };
} else {
  module.exports = process
}

function nextTick(fn, arg1, arg2, arg3) {
  if (typeof fn !== 'function') {
    throw new TypeError('"callback" argument must be a function');
  }
  var len = arguments.length;
  var args, i;
  switch (len) {
  case 0:
  case 1:
    return process.nextTick(fn);
  case 2:
    return process.nextTick(function afterTickOne() {
      fn.call(null, arg1);
    });
  case 3:
    return process.nextTick(function afterTickTwo() {
      fn.call(null, arg1, arg2);
    });
  case 4:
    return process.nextTick(function afterTickThree() {
      fn.call(null, arg1, arg2, arg3);
    });
  default:
    args = new Array(len - 1);
    i = 0;
    while (i < args.length) {
      args[i++] = arguments[i];
    }
    return process.nextTick(function afterTick() {
      fn.apply(null, args);
    });
  }
}


}).call(this,require('_process'))
},{"_process":46}],46:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) { return [] }

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}],47:[function(require,module,exports){
module.exports = require('./lib/_stream_duplex.js');

},{"./lib/_stream_duplex.js":48}],48:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a duplex stream is just a stream that is both readable and writable.
// Since JS doesn't have multiple prototypal inheritance, this class
// prototypally inherits from Readable, and then parasitically from
// Writable.

'use strict';

/*<replacement>*/

var pna = require('process-nextick-args');
/*</replacement>*/

/*<replacement>*/
var objectKeys = Object.keys || function (obj) {
  var keys = [];
  for (var key in obj) {
    keys.push(key);
  }return keys;
};
/*</replacement>*/

module.exports = Duplex;

/*<replacement>*/
var util = Object.create(require('core-util-is'));
util.inherits = require('inherits');
/*</replacement>*/

var Readable = require('./_stream_readable');
var Writable = require('./_stream_writable');

util.inherits(Duplex, Readable);

{
  // avoid scope creep, the keys array can then be collected
  var keys = objectKeys(Writable.prototype);
  for (var v = 0; v < keys.length; v++) {
    var method = keys[v];
    if (!Duplex.prototype[method]) Duplex.prototype[method] = Writable.prototype[method];
  }
}

function Duplex(options) {
  if (!(this instanceof Duplex)) return new Duplex(options);

  Readable.call(this, options);
  Writable.call(this, options);

  if (options && options.readable === false) this.readable = false;

  if (options && options.writable === false) this.writable = false;

  this.allowHalfOpen = true;
  if (options && options.allowHalfOpen === false) this.allowHalfOpen = false;

  this.once('end', onend);
}

Object.defineProperty(Duplex.prototype, 'writableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function () {
    return this._writableState.highWaterMark;
  }
});

// the no-half-open enforcer
function onend() {
  // if we allow half-open state, or if the writable side ended,
  // then we're ok.
  if (this.allowHalfOpen || this._writableState.ended) return;

  // no more data can be written.
  // But allow more writes to happen in this tick.
  pna.nextTick(onEndNT, this);
}

function onEndNT(self) {
  self.end();
}

Object.defineProperty(Duplex.prototype, 'destroyed', {
  get: function () {
    if (this._readableState === undefined || this._writableState === undefined) {
      return false;
    }
    return this._readableState.destroyed && this._writableState.destroyed;
  },
  set: function (value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (this._readableState === undefined || this._writableState === undefined) {
      return;
    }

    // backward compatibility, the user is explicitly
    // managing destroyed
    this._readableState.destroyed = value;
    this._writableState.destroyed = value;
  }
});

Duplex.prototype._destroy = function (err, cb) {
  this.push(null);
  this.end();

  pna.nextTick(cb, err);
};
},{"./_stream_readable":50,"./_stream_writable":52,"core-util-is":38,"inherits":41,"process-nextick-args":45}],49:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a passthrough stream.
// basically just the most minimal sort of Transform stream.
// Every written chunk gets output as-is.

'use strict';

module.exports = PassThrough;

var Transform = require('./_stream_transform');

/*<replacement>*/
var util = Object.create(require('core-util-is'));
util.inherits = require('inherits');
/*</replacement>*/

util.inherits(PassThrough, Transform);

function PassThrough(options) {
  if (!(this instanceof PassThrough)) return new PassThrough(options);

  Transform.call(this, options);
}

PassThrough.prototype._transform = function (chunk, encoding, cb) {
  cb(null, chunk);
};
},{"./_stream_transform":51,"core-util-is":38,"inherits":41}],50:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

/*<replacement>*/

var pna = require('process-nextick-args');
/*</replacement>*/

module.exports = Readable;

/*<replacement>*/
var isArray = require('isarray');
/*</replacement>*/

/*<replacement>*/
var Duplex;
/*</replacement>*/

Readable.ReadableState = ReadableState;

/*<replacement>*/
var EE = require('events').EventEmitter;

var EElistenerCount = function (emitter, type) {
  return emitter.listeners(type).length;
};
/*</replacement>*/

/*<replacement>*/
var Stream = require('./internal/streams/stream');
/*</replacement>*/

/*<replacement>*/

var Buffer = require('safe-buffer').Buffer;
var OurUint8Array = global.Uint8Array || function () {};
function _uint8ArrayToBuffer(chunk) {
  return Buffer.from(chunk);
}
function _isUint8Array(obj) {
  return Buffer.isBuffer(obj) || obj instanceof OurUint8Array;
}

/*</replacement>*/

/*<replacement>*/
var util = Object.create(require('core-util-is'));
util.inherits = require('inherits');
/*</replacement>*/

/*<replacement>*/
var debugUtil = require('util');
var debug = void 0;
if (debugUtil && debugUtil.debuglog) {
  debug = debugUtil.debuglog('stream');
} else {
  debug = function () {};
}
/*</replacement>*/

var BufferList = require('./internal/streams/BufferList');
var destroyImpl = require('./internal/streams/destroy');
var StringDecoder;

util.inherits(Readable, Stream);

var kProxyEvents = ['error', 'close', 'destroy', 'pause', 'resume'];

function prependListener(emitter, event, fn) {
  // Sadly this is not cacheable as some libraries bundle their own
  // event emitter implementation with them.
  if (typeof emitter.prependListener === 'function') return emitter.prependListener(event, fn);

  // This is a hack to make sure that our error handler is attached before any
  // userland ones.  NEVER DO THIS. This is here only because this code needs
  // to continue to work with older versions of Node.js that do not include
  // the prependListener() method. The goal is to eventually remove this hack.
  if (!emitter._events || !emitter._events[event]) emitter.on(event, fn);else if (isArray(emitter._events[event])) emitter._events[event].unshift(fn);else emitter._events[event] = [fn, emitter._events[event]];
}

function ReadableState(options, stream) {
  Duplex = Duplex || require('./_stream_duplex');

  options = options || {};

  // Duplex streams are both readable and writable, but share
  // the same options object.
  // However, some cases require setting options to different
  // values for the readable and the writable sides of the duplex stream.
  // These options can be provided separately as readableXXX and writableXXX.
  var isDuplex = stream instanceof Duplex;

  // object stream flag. Used to make read(n) ignore n and to
  // make all the buffer merging and length checks go away
  this.objectMode = !!options.objectMode;

  if (isDuplex) this.objectMode = this.objectMode || !!options.readableObjectMode;

  // the point at which it stops calling _read() to fill the buffer
  // Note: 0 is a valid value, means "don't call _read preemptively ever"
  var hwm = options.highWaterMark;
  var readableHwm = options.readableHighWaterMark;
  var defaultHwm = this.objectMode ? 16 : 16 * 1024;

  if (hwm || hwm === 0) this.highWaterMark = hwm;else if (isDuplex && (readableHwm || readableHwm === 0)) this.highWaterMark = readableHwm;else this.highWaterMark = defaultHwm;

  // cast to ints.
  this.highWaterMark = Math.floor(this.highWaterMark);

  // A linked list is used to store data chunks instead of an array because the
  // linked list can remove elements from the beginning faster than
  // array.shift()
  this.buffer = new BufferList();
  this.length = 0;
  this.pipes = null;
  this.pipesCount = 0;
  this.flowing = null;
  this.ended = false;
  this.endEmitted = false;
  this.reading = false;

  // a flag to be able to tell if the event 'readable'/'data' is emitted
  // immediately, or on a later tick.  We set this to true at first, because
  // any actions that shouldn't happen until "later" should generally also
  // not happen before the first read call.
  this.sync = true;

  // whenever we return null, then we set a flag to say
  // that we're awaiting a 'readable' event emission.
  this.needReadable = false;
  this.emittedReadable = false;
  this.readableListening = false;
  this.resumeScheduled = false;

  // has it been destroyed
  this.destroyed = false;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // the number of writers that are awaiting a drain event in .pipe()s
  this.awaitDrain = 0;

  // if true, a maybeReadMore has been scheduled
  this.readingMore = false;

  this.decoder = null;
  this.encoding = null;
  if (options.encoding) {
    if (!StringDecoder) StringDecoder = require('string_decoder/').StringDecoder;
    this.decoder = new StringDecoder(options.encoding);
    this.encoding = options.encoding;
  }
}

function Readable(options) {
  Duplex = Duplex || require('./_stream_duplex');

  if (!(this instanceof Readable)) return new Readable(options);

  this._readableState = new ReadableState(options, this);

  // legacy
  this.readable = true;

  if (options) {
    if (typeof options.read === 'function') this._read = options.read;

    if (typeof options.destroy === 'function') this._destroy = options.destroy;
  }

  Stream.call(this);
}

Object.defineProperty(Readable.prototype, 'destroyed', {
  get: function () {
    if (this._readableState === undefined) {
      return false;
    }
    return this._readableState.destroyed;
  },
  set: function (value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (!this._readableState) {
      return;
    }

    // backward compatibility, the user is explicitly
    // managing destroyed
    this._readableState.destroyed = value;
  }
});

Readable.prototype.destroy = destroyImpl.destroy;
Readable.prototype._undestroy = destroyImpl.undestroy;
Readable.prototype._destroy = function (err, cb) {
  this.push(null);
  cb(err);
};

// Manually shove something into the read() buffer.
// This returns true if the highWaterMark has not been hit yet,
// similar to how Writable.write() returns true if you should
// write() some more.
Readable.prototype.push = function (chunk, encoding) {
  var state = this._readableState;
  var skipChunkCheck;

  if (!state.objectMode) {
    if (typeof chunk === 'string') {
      encoding = encoding || state.defaultEncoding;
      if (encoding !== state.encoding) {
        chunk = Buffer.from(chunk, encoding);
        encoding = '';
      }
      skipChunkCheck = true;
    }
  } else {
    skipChunkCheck = true;
  }

  return readableAddChunk(this, chunk, encoding, false, skipChunkCheck);
};

// Unshift should *always* be something directly out of read()
Readable.prototype.unshift = function (chunk) {
  return readableAddChunk(this, chunk, null, true, false);
};

function readableAddChunk(stream, chunk, encoding, addToFront, skipChunkCheck) {
  var state = stream._readableState;
  if (chunk === null) {
    state.reading = false;
    onEofChunk(stream, state);
  } else {
    var er;
    if (!skipChunkCheck) er = chunkInvalid(state, chunk);
    if (er) {
      stream.emit('error', er);
    } else if (state.objectMode || chunk && chunk.length > 0) {
      if (typeof chunk !== 'string' && !state.objectMode && Object.getPrototypeOf(chunk) !== Buffer.prototype) {
        chunk = _uint8ArrayToBuffer(chunk);
      }

      if (addToFront) {
        if (state.endEmitted) stream.emit('error', new Error('stream.unshift() after end event'));else addChunk(stream, state, chunk, true);
      } else if (state.ended) {
        stream.emit('error', new Error('stream.push() after EOF'));
      } else {
        state.reading = false;
        if (state.decoder && !encoding) {
          chunk = state.decoder.write(chunk);
          if (state.objectMode || chunk.length !== 0) addChunk(stream, state, chunk, false);else maybeReadMore(stream, state);
        } else {
          addChunk(stream, state, chunk, false);
        }
      }
    } else if (!addToFront) {
      state.reading = false;
    }
  }

  return needMoreData(state);
}

function addChunk(stream, state, chunk, addToFront) {
  if (state.flowing && state.length === 0 && !state.sync) {
    stream.emit('data', chunk);
    stream.read(0);
  } else {
    // update the buffer info.
    state.length += state.objectMode ? 1 : chunk.length;
    if (addToFront) state.buffer.unshift(chunk);else state.buffer.push(chunk);

    if (state.needReadable) emitReadable(stream);
  }
  maybeReadMore(stream, state);
}

function chunkInvalid(state, chunk) {
  var er;
  if (!_isUint8Array(chunk) && typeof chunk !== 'string' && chunk !== undefined && !state.objectMode) {
    er = new TypeError('Invalid non-string/buffer chunk');
  }
  return er;
}

// if it's past the high water mark, we can push in some more.
// Also, if we have no data yet, we can stand some
// more bytes.  This is to work around cases where hwm=0,
// such as the repl.  Also, if the push() triggered a
// readable event, and the user called read(largeNumber) such that
// needReadable was set, then we ought to push more, so that another
// 'readable' event will be triggered.
function needMoreData(state) {
  return !state.ended && (state.needReadable || state.length < state.highWaterMark || state.length === 0);
}

Readable.prototype.isPaused = function () {
  return this._readableState.flowing === false;
};

// backwards compatibility.
Readable.prototype.setEncoding = function (enc) {
  if (!StringDecoder) StringDecoder = require('string_decoder/').StringDecoder;
  this._readableState.decoder = new StringDecoder(enc);
  this._readableState.encoding = enc;
  return this;
};

// Don't raise the hwm > 8MB
var MAX_HWM = 0x800000;
function computeNewHighWaterMark(n) {
  if (n >= MAX_HWM) {
    n = MAX_HWM;
  } else {
    // Get the next highest power of 2 to prevent increasing hwm excessively in
    // tiny amounts
    n--;
    n |= n >>> 1;
    n |= n >>> 2;
    n |= n >>> 4;
    n |= n >>> 8;
    n |= n >>> 16;
    n++;
  }
  return n;
}

// This function is designed to be inlinable, so please take care when making
// changes to the function body.
function howMuchToRead(n, state) {
  if (n <= 0 || state.length === 0 && state.ended) return 0;
  if (state.objectMode) return 1;
  if (n !== n) {
    // Only flow one buffer at a time
    if (state.flowing && state.length) return state.buffer.head.data.length;else return state.length;
  }
  // If we're asking for more than the current hwm, then raise the hwm.
  if (n > state.highWaterMark) state.highWaterMark = computeNewHighWaterMark(n);
  if (n <= state.length) return n;
  // Don't have enough
  if (!state.ended) {
    state.needReadable = true;
    return 0;
  }
  return state.length;
}

// you can override either this method, or the async _read(n) below.
Readable.prototype.read = function (n) {
  debug('read', n);
  n = parseInt(n, 10);
  var state = this._readableState;
  var nOrig = n;

  if (n !== 0) state.emittedReadable = false;

  // if we're doing read(0) to trigger a readable event, but we
  // already have a bunch of data in the buffer, then just trigger
  // the 'readable' event and move on.
  if (n === 0 && state.needReadable && (state.length >= state.highWaterMark || state.ended)) {
    debug('read: emitReadable', state.length, state.ended);
    if (state.length === 0 && state.ended) endReadable(this);else emitReadable(this);
    return null;
  }

  n = howMuchToRead(n, state);

  // if we've ended, and we're now clear, then finish it up.
  if (n === 0 && state.ended) {
    if (state.length === 0) endReadable(this);
    return null;
  }

  // All the actual chunk generation logic needs to be
  // *below* the call to _read.  The reason is that in certain
  // synthetic stream cases, such as passthrough streams, _read
  // may be a completely synchronous operation which may change
  // the state of the read buffer, providing enough data when
  // before there was *not* enough.
  //
  // So, the steps are:
  // 1. Figure out what the state of things will be after we do
  // a read from the buffer.
  //
  // 2. If that resulting state will trigger a _read, then call _read.
  // Note that this may be asynchronous, or synchronous.  Yes, it is
  // deeply ugly to write APIs this way, but that still doesn't mean
  // that the Readable class should behave improperly, as streams are
  // designed to be sync/async agnostic.
  // Take note if the _read call is sync or async (ie, if the read call
  // has returned yet), so that we know whether or not it's safe to emit
  // 'readable' etc.
  //
  // 3. Actually pull the requested chunks out of the buffer and return.

  // if we need a readable event, then we need to do some reading.
  var doRead = state.needReadable;
  debug('need readable', doRead);

  // if we currently have less than the highWaterMark, then also read some
  if (state.length === 0 || state.length - n < state.highWaterMark) {
    doRead = true;
    debug('length less than watermark', doRead);
  }

  // however, if we've ended, then there's no point, and if we're already
  // reading, then it's unnecessary.
  if (state.ended || state.reading) {
    doRead = false;
    debug('reading or ended', doRead);
  } else if (doRead) {
    debug('do read');
    state.reading = true;
    state.sync = true;
    // if the length is currently zero, then we *need* a readable event.
    if (state.length === 0) state.needReadable = true;
    // call internal read method
    this._read(state.highWaterMark);
    state.sync = false;
    // If _read pushed data synchronously, then `reading` will be false,
    // and we need to re-evaluate how much data we can return to the user.
    if (!state.reading) n = howMuchToRead(nOrig, state);
  }

  var ret;
  if (n > 0) ret = fromList(n, state);else ret = null;

  if (ret === null) {
    state.needReadable = true;
    n = 0;
  } else {
    state.length -= n;
  }

  if (state.length === 0) {
    // If we have nothing in the buffer, then we want to know
    // as soon as we *do* get something into the buffer.
    if (!state.ended) state.needReadable = true;

    // If we tried to read() past the EOF, then emit end on the next tick.
    if (nOrig !== n && state.ended) endReadable(this);
  }

  if (ret !== null) this.emit('data', ret);

  return ret;
};

function onEofChunk(stream, state) {
  if (state.ended) return;
  if (state.decoder) {
    var chunk = state.decoder.end();
    if (chunk && chunk.length) {
      state.buffer.push(chunk);
      state.length += state.objectMode ? 1 : chunk.length;
    }
  }
  state.ended = true;

  // emit 'readable' now to make sure it gets picked up.
  emitReadable(stream);
}

// Don't emit readable right away in sync mode, because this can trigger
// another read() call => stack overflow.  This way, it might trigger
// a nextTick recursion warning, but that's not so bad.
function emitReadable(stream) {
  var state = stream._readableState;
  state.needReadable = false;
  if (!state.emittedReadable) {
    debug('emitReadable', state.flowing);
    state.emittedReadable = true;
    if (state.sync) pna.nextTick(emitReadable_, stream);else emitReadable_(stream);
  }
}

function emitReadable_(stream) {
  debug('emit readable');
  stream.emit('readable');
  flow(stream);
}

// at this point, the user has presumably seen the 'readable' event,
// and called read() to consume some data.  that may have triggered
// in turn another _read(n) call, in which case reading = true if
// it's in progress.
// However, if we're not ended, or reading, and the length < hwm,
// then go ahead and try to read some more preemptively.
function maybeReadMore(stream, state) {
  if (!state.readingMore) {
    state.readingMore = true;
    pna.nextTick(maybeReadMore_, stream, state);
  }
}

function maybeReadMore_(stream, state) {
  var len = state.length;
  while (!state.reading && !state.flowing && !state.ended && state.length < state.highWaterMark) {
    debug('maybeReadMore read 0');
    stream.read(0);
    if (len === state.length)
      // didn't get any data, stop spinning.
      break;else len = state.length;
  }
  state.readingMore = false;
}

// abstract method.  to be overridden in specific implementation classes.
// call cb(er, data) where data is <= n in length.
// for virtual (non-string, non-buffer) streams, "length" is somewhat
// arbitrary, and perhaps not very meaningful.
Readable.prototype._read = function (n) {
  this.emit('error', new Error('_read() is not implemented'));
};

Readable.prototype.pipe = function (dest, pipeOpts) {
  var src = this;
  var state = this._readableState;

  switch (state.pipesCount) {
    case 0:
      state.pipes = dest;
      break;
    case 1:
      state.pipes = [state.pipes, dest];
      break;
    default:
      state.pipes.push(dest);
      break;
  }
  state.pipesCount += 1;
  debug('pipe count=%d opts=%j', state.pipesCount, pipeOpts);

  var doEnd = (!pipeOpts || pipeOpts.end !== false) && dest !== process.stdout && dest !== process.stderr;

  var endFn = doEnd ? onend : unpipe;
  if (state.endEmitted) pna.nextTick(endFn);else src.once('end', endFn);

  dest.on('unpipe', onunpipe);
  function onunpipe(readable, unpipeInfo) {
    debug('onunpipe');
    if (readable === src) {
      if (unpipeInfo && unpipeInfo.hasUnpiped === false) {
        unpipeInfo.hasUnpiped = true;
        cleanup();
      }
    }
  }

  function onend() {
    debug('onend');
    dest.end();
  }

  // when the dest drains, it reduces the awaitDrain counter
  // on the source.  This would be more elegant with a .once()
  // handler in flow(), but adding and removing repeatedly is
  // too slow.
  var ondrain = pipeOnDrain(src);
  dest.on('drain', ondrain);

  var cleanedUp = false;
  function cleanup() {
    debug('cleanup');
    // cleanup event handlers once the pipe is broken
    dest.removeListener('close', onclose);
    dest.removeListener('finish', onfinish);
    dest.removeListener('drain', ondrain);
    dest.removeListener('error', onerror);
    dest.removeListener('unpipe', onunpipe);
    src.removeListener('end', onend);
    src.removeListener('end', unpipe);
    src.removeListener('data', ondata);

    cleanedUp = true;

    // if the reader is waiting for a drain event from this
    // specific writer, then it would cause it to never start
    // flowing again.
    // So, if this is awaiting a drain, then we just call it now.
    // If we don't know, then assume that we are waiting for one.
    if (state.awaitDrain && (!dest._writableState || dest._writableState.needDrain)) ondrain();
  }

  // If the user pushes more data while we're writing to dest then we'll end up
  // in ondata again. However, we only want to increase awaitDrain once because
  // dest will only emit one 'drain' event for the multiple writes.
  // => Introduce a guard on increasing awaitDrain.
  var increasedAwaitDrain = false;
  src.on('data', ondata);
  function ondata(chunk) {
    debug('ondata');
    increasedAwaitDrain = false;
    var ret = dest.write(chunk);
    if (false === ret && !increasedAwaitDrain) {
      // If the user unpiped during `dest.write()`, it is possible
      // to get stuck in a permanently paused state if that write
      // also returned false.
      // => Check whether `dest` is still a piping destination.
      if ((state.pipesCount === 1 && state.pipes === dest || state.pipesCount > 1 && indexOf(state.pipes, dest) !== -1) && !cleanedUp) {
        debug('false write response, pause', src._readableState.awaitDrain);
        src._readableState.awaitDrain++;
        increasedAwaitDrain = true;
      }
      src.pause();
    }
  }

  // if the dest has an error, then stop piping into it.
  // however, don't suppress the throwing behavior for this.
  function onerror(er) {
    debug('onerror', er);
    unpipe();
    dest.removeListener('error', onerror);
    if (EElistenerCount(dest, 'error') === 0) dest.emit('error', er);
  }

  // Make sure our error handler is attached before userland ones.
  prependListener(dest, 'error', onerror);

  // Both close and finish should trigger unpipe, but only once.
  function onclose() {
    dest.removeListener('finish', onfinish);
    unpipe();
  }
  dest.once('close', onclose);
  function onfinish() {
    debug('onfinish');
    dest.removeListener('close', onclose);
    unpipe();
  }
  dest.once('finish', onfinish);

  function unpipe() {
    debug('unpipe');
    src.unpipe(dest);
  }

  // tell the dest that it's being piped to
  dest.emit('pipe', src);

  // start the flow if it hasn't been started already.
  if (!state.flowing) {
    debug('pipe resume');
    src.resume();
  }

  return dest;
};

function pipeOnDrain(src) {
  return function () {
    var state = src._readableState;
    debug('pipeOnDrain', state.awaitDrain);
    if (state.awaitDrain) state.awaitDrain--;
    if (state.awaitDrain === 0 && EElistenerCount(src, 'data')) {
      state.flowing = true;
      flow(src);
    }
  };
}

Readable.prototype.unpipe = function (dest) {
  var state = this._readableState;
  var unpipeInfo = { hasUnpiped: false };

  // if we're not piping anywhere, then do nothing.
  if (state.pipesCount === 0) return this;

  // just one destination.  most common case.
  if (state.pipesCount === 1) {
    // passed in one, but it's not the right one.
    if (dest && dest !== state.pipes) return this;

    if (!dest) dest = state.pipes;

    // got a match.
    state.pipes = null;
    state.pipesCount = 0;
    state.flowing = false;
    if (dest) dest.emit('unpipe', this, unpipeInfo);
    return this;
  }

  // slow case. multiple pipe destinations.

  if (!dest) {
    // remove all.
    var dests = state.pipes;
    var len = state.pipesCount;
    state.pipes = null;
    state.pipesCount = 0;
    state.flowing = false;

    for (var i = 0; i < len; i++) {
      dests[i].emit('unpipe', this, unpipeInfo);
    }return this;
  }

  // try to find the right one.
  var index = indexOf(state.pipes, dest);
  if (index === -1) return this;

  state.pipes.splice(index, 1);
  state.pipesCount -= 1;
  if (state.pipesCount === 1) state.pipes = state.pipes[0];

  dest.emit('unpipe', this, unpipeInfo);

  return this;
};

// set up data events if they are asked for
// Ensure readable listeners eventually get something
Readable.prototype.on = function (ev, fn) {
  var res = Stream.prototype.on.call(this, ev, fn);

  if (ev === 'data') {
    // Start flowing on next tick if stream isn't explicitly paused
    if (this._readableState.flowing !== false) this.resume();
  } else if (ev === 'readable') {
    var state = this._readableState;
    if (!state.endEmitted && !state.readableListening) {
      state.readableListening = state.needReadable = true;
      state.emittedReadable = false;
      if (!state.reading) {
        pna.nextTick(nReadingNextTick, this);
      } else if (state.length) {
        emitReadable(this);
      }
    }
  }

  return res;
};
Readable.prototype.addListener = Readable.prototype.on;

function nReadingNextTick(self) {
  debug('readable nexttick read 0');
  self.read(0);
}

// pause() and resume() are remnants of the legacy readable stream API
// If the user uses them, then switch into old mode.
Readable.prototype.resume = function () {
  var state = this._readableState;
  if (!state.flowing) {
    debug('resume');
    state.flowing = true;
    resume(this, state);
  }
  return this;
};

function resume(stream, state) {
  if (!state.resumeScheduled) {
    state.resumeScheduled = true;
    pna.nextTick(resume_, stream, state);
  }
}

function resume_(stream, state) {
  if (!state.reading) {
    debug('resume read 0');
    stream.read(0);
  }

  state.resumeScheduled = false;
  state.awaitDrain = 0;
  stream.emit('resume');
  flow(stream);
  if (state.flowing && !state.reading) stream.read(0);
}

Readable.prototype.pause = function () {
  debug('call pause flowing=%j', this._readableState.flowing);
  if (false !== this._readableState.flowing) {
    debug('pause');
    this._readableState.flowing = false;
    this.emit('pause');
  }
  return this;
};

function flow(stream) {
  var state = stream._readableState;
  debug('flow', state.flowing);
  while (state.flowing && stream.read() !== null) {}
}

// wrap an old-style stream as the async data source.
// This is *not* part of the readable stream interface.
// It is an ugly unfortunate mess of history.
Readable.prototype.wrap = function (stream) {
  var _this = this;

  var state = this._readableState;
  var paused = false;

  stream.on('end', function () {
    debug('wrapped end');
    if (state.decoder && !state.ended) {
      var chunk = state.decoder.end();
      if (chunk && chunk.length) _this.push(chunk);
    }

    _this.push(null);
  });

  stream.on('data', function (chunk) {
    debug('wrapped data');
    if (state.decoder) chunk = state.decoder.write(chunk);

    // don't skip over falsy values in objectMode
    if (state.objectMode && (chunk === null || chunk === undefined)) return;else if (!state.objectMode && (!chunk || !chunk.length)) return;

    var ret = _this.push(chunk);
    if (!ret) {
      paused = true;
      stream.pause();
    }
  });

  // proxy all the other methods.
  // important when wrapping filters and duplexes.
  for (var i in stream) {
    if (this[i] === undefined && typeof stream[i] === 'function') {
      this[i] = function (method) {
        return function () {
          return stream[method].apply(stream, arguments);
        };
      }(i);
    }
  }

  // proxy certain important events.
  for (var n = 0; n < kProxyEvents.length; n++) {
    stream.on(kProxyEvents[n], this.emit.bind(this, kProxyEvents[n]));
  }

  // when we try to consume some more bytes, simply unpause the
  // underlying stream.
  this._read = function (n) {
    debug('wrapped _read', n);
    if (paused) {
      paused = false;
      stream.resume();
    }
  };

  return this;
};

Object.defineProperty(Readable.prototype, 'readableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function () {
    return this._readableState.highWaterMark;
  }
});

// exposed for testing purposes only.
Readable._fromList = fromList;

// Pluck off n bytes from an array of buffers.
// Length is the combined lengths of all the buffers in the list.
// This function is designed to be inlinable, so please take care when making
// changes to the function body.
function fromList(n, state) {
  // nothing buffered
  if (state.length === 0) return null;

  var ret;
  if (state.objectMode) ret = state.buffer.shift();else if (!n || n >= state.length) {
    // read it all, truncate the list
    if (state.decoder) ret = state.buffer.join('');else if (state.buffer.length === 1) ret = state.buffer.head.data;else ret = state.buffer.concat(state.length);
    state.buffer.clear();
  } else {
    // read part of list
    ret = fromListPartial(n, state.buffer, state.decoder);
  }

  return ret;
}

// Extracts only enough buffered data to satisfy the amount requested.
// This function is designed to be inlinable, so please take care when making
// changes to the function body.
function fromListPartial(n, list, hasStrings) {
  var ret;
  if (n < list.head.data.length) {
    // slice is the same for buffers and strings
    ret = list.head.data.slice(0, n);
    list.head.data = list.head.data.slice(n);
  } else if (n === list.head.data.length) {
    // first chunk is a perfect match
    ret = list.shift();
  } else {
    // result spans more than one buffer
    ret = hasStrings ? copyFromBufferString(n, list) : copyFromBuffer(n, list);
  }
  return ret;
}

// Copies a specified amount of characters from the list of buffered data
// chunks.
// This function is designed to be inlinable, so please take care when making
// changes to the function body.
function copyFromBufferString(n, list) {
  var p = list.head;
  var c = 1;
  var ret = p.data;
  n -= ret.length;
  while (p = p.next) {
    var str = p.data;
    var nb = n > str.length ? str.length : n;
    if (nb === str.length) ret += str;else ret += str.slice(0, n);
    n -= nb;
    if (n === 0) {
      if (nb === str.length) {
        ++c;
        if (p.next) list.head = p.next;else list.head = list.tail = null;
      } else {
        list.head = p;
        p.data = str.slice(nb);
      }
      break;
    }
    ++c;
  }
  list.length -= c;
  return ret;
}

// Copies a specified amount of bytes from the list of buffered data chunks.
// This function is designed to be inlinable, so please take care when making
// changes to the function body.
function copyFromBuffer(n, list) {
  var ret = Buffer.allocUnsafe(n);
  var p = list.head;
  var c = 1;
  p.data.copy(ret);
  n -= p.data.length;
  while (p = p.next) {
    var buf = p.data;
    var nb = n > buf.length ? buf.length : n;
    buf.copy(ret, ret.length - n, 0, nb);
    n -= nb;
    if (n === 0) {
      if (nb === buf.length) {
        ++c;
        if (p.next) list.head = p.next;else list.head = list.tail = null;
      } else {
        list.head = p;
        p.data = buf.slice(nb);
      }
      break;
    }
    ++c;
  }
  list.length -= c;
  return ret;
}

function endReadable(stream) {
  var state = stream._readableState;

  // If we get here before consuming all the bytes, then that is a
  // bug in node.  Should never happen.
  if (state.length > 0) throw new Error('"endReadable()" called on non-empty stream');

  if (!state.endEmitted) {
    state.ended = true;
    pna.nextTick(endReadableNT, state, stream);
  }
}

function endReadableNT(state, stream) {
  // Check that we didn't get one last unshift.
  if (!state.endEmitted && state.length === 0) {
    state.endEmitted = true;
    stream.readable = false;
    stream.emit('end');
  }
}

function indexOf(xs, x) {
  for (var i = 0, l = xs.length; i < l; i++) {
    if (xs[i] === x) return i;
  }
  return -1;
}
}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./_stream_duplex":48,"./internal/streams/BufferList":53,"./internal/streams/destroy":54,"./internal/streams/stream":55,"_process":46,"core-util-is":38,"events":39,"inherits":41,"isarray":43,"process-nextick-args":45,"safe-buffer":56,"string_decoder/":57,"util":36}],51:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// a transform stream is a readable/writable stream where you do
// something with the data.  Sometimes it's called a "filter",
// but that's not a great name for it, since that implies a thing where
// some bits pass through, and others are simply ignored.  (That would
// be a valid example of a transform, of course.)
//
// While the output is causally related to the input, it's not a
// necessarily symmetric or synchronous transformation.  For example,
// a zlib stream might take multiple plain-text writes(), and then
// emit a single compressed chunk some time in the future.
//
// Here's how this works:
//
// The Transform stream has all the aspects of the readable and writable
// stream classes.  When you write(chunk), that calls _write(chunk,cb)
// internally, and returns false if there's a lot of pending writes
// buffered up.  When you call read(), that calls _read(n) until
// there's enough pending readable data buffered up.
//
// In a transform stream, the written data is placed in a buffer.  When
// _read(n) is called, it transforms the queued up data, calling the
// buffered _write cb's as it consumes chunks.  If consuming a single
// written chunk would result in multiple output chunks, then the first
// outputted bit calls the readcb, and subsequent chunks just go into
// the read buffer, and will cause it to emit 'readable' if necessary.
//
// This way, back-pressure is actually determined by the reading side,
// since _read has to be called to start processing a new chunk.  However,
// a pathological inflate type of transform can cause excessive buffering
// here.  For example, imagine a stream where every byte of input is
// interpreted as an integer from 0-255, and then results in that many
// bytes of output.  Writing the 4 bytes {ff,ff,ff,ff} would result in
// 1kb of data being output.  In this case, you could write a very small
// amount of input, and end up with a very large amount of output.  In
// such a pathological inflating mechanism, there'd be no way to tell
// the system to stop doing the transform.  A single 4MB write could
// cause the system to run out of memory.
//
// However, even in such a pathological case, only a single written chunk
// would be consumed, and then the rest would wait (un-transformed) until
// the results of the previous transformed chunk were consumed.

'use strict';

module.exports = Transform;

var Duplex = require('./_stream_duplex');

/*<replacement>*/
var util = Object.create(require('core-util-is'));
util.inherits = require('inherits');
/*</replacement>*/

util.inherits(Transform, Duplex);

function afterTransform(er, data) {
  var ts = this._transformState;
  ts.transforming = false;

  var cb = ts.writecb;

  if (!cb) {
    return this.emit('error', new Error('write callback called multiple times'));
  }

  ts.writechunk = null;
  ts.writecb = null;

  if (data != null) // single equals check for both `null` and `undefined`
    this.push(data);

  cb(er);

  var rs = this._readableState;
  rs.reading = false;
  if (rs.needReadable || rs.length < rs.highWaterMark) {
    this._read(rs.highWaterMark);
  }
}

function Transform(options) {
  if (!(this instanceof Transform)) return new Transform(options);

  Duplex.call(this, options);

  this._transformState = {
    afterTransform: afterTransform.bind(this),
    needTransform: false,
    transforming: false,
    writecb: null,
    writechunk: null,
    writeencoding: null
  };

  // start out asking for a readable event once data is transformed.
  this._readableState.needReadable = true;

  // we have implemented the _read method, and done the other things
  // that Readable wants before the first _read call, so unset the
  // sync guard flag.
  this._readableState.sync = false;

  if (options) {
    if (typeof options.transform === 'function') this._transform = options.transform;

    if (typeof options.flush === 'function') this._flush = options.flush;
  }

  // When the writable side finishes, then flush out anything remaining.
  this.on('prefinish', prefinish);
}

function prefinish() {
  var _this = this;

  if (typeof this._flush === 'function') {
    this._flush(function (er, data) {
      done(_this, er, data);
    });
  } else {
    done(this, null, null);
  }
}

Transform.prototype.push = function (chunk, encoding) {
  this._transformState.needTransform = false;
  return Duplex.prototype.push.call(this, chunk, encoding);
};

// This is the part where you do stuff!
// override this function in implementation classes.
// 'chunk' is an input chunk.
//
// Call `push(newChunk)` to pass along transformed output
// to the readable side.  You may call 'push' zero or more times.
//
// Call `cb(err)` when you are done with this chunk.  If you pass
// an error, then that'll put the hurt on the whole operation.  If you
// never call cb(), then you'll never get another chunk.
Transform.prototype._transform = function (chunk, encoding, cb) {
  throw new Error('_transform() is not implemented');
};

Transform.prototype._write = function (chunk, encoding, cb) {
  var ts = this._transformState;
  ts.writecb = cb;
  ts.writechunk = chunk;
  ts.writeencoding = encoding;
  if (!ts.transforming) {
    var rs = this._readableState;
    if (ts.needTransform || rs.needReadable || rs.length < rs.highWaterMark) this._read(rs.highWaterMark);
  }
};

// Doesn't matter what the args are here.
// _transform does all the work.
// That we got here means that the readable side wants more data.
Transform.prototype._read = function (n) {
  var ts = this._transformState;

  if (ts.writechunk !== null && ts.writecb && !ts.transforming) {
    ts.transforming = true;
    this._transform(ts.writechunk, ts.writeencoding, ts.afterTransform);
  } else {
    // mark that we need a transform, so that any data that comes in
    // will get processed, now that we've asked for it.
    ts.needTransform = true;
  }
};

Transform.prototype._destroy = function (err, cb) {
  var _this2 = this;

  Duplex.prototype._destroy.call(this, err, function (err2) {
    cb(err2);
    _this2.emit('close');
  });
};

function done(stream, er, data) {
  if (er) return stream.emit('error', er);

  if (data != null) // single equals check for both `null` and `undefined`
    stream.push(data);

  // if there's nothing in the write buffer, then that means
  // that nothing more will ever be provided
  if (stream._writableState.length) throw new Error('Calling transform done when ws.length != 0');

  if (stream._transformState.transforming) throw new Error('Calling transform done when still transforming');

  return stream.push(null);
}
},{"./_stream_duplex":48,"core-util-is":38,"inherits":41}],52:[function(require,module,exports){
(function (process,global,setImmediate){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// A bit simpler than readable streams.
// Implement an async ._write(chunk, encoding, cb), and it'll handle all
// the drain event emission and buffering.

'use strict';

/*<replacement>*/

var pna = require('process-nextick-args');
/*</replacement>*/

module.exports = Writable;

/* <replacement> */
function WriteReq(chunk, encoding, cb) {
  this.chunk = chunk;
  this.encoding = encoding;
  this.callback = cb;
  this.next = null;
}

// It seems a linked list but it is not
// there will be only 2 of these for each stream
function CorkedRequest(state) {
  var _this = this;

  this.next = null;
  this.entry = null;
  this.finish = function () {
    onCorkedFinish(_this, state);
  };
}
/* </replacement> */

/*<replacement>*/
var asyncWrite = !process.browser && ['v0.10', 'v0.9.'].indexOf(process.version.slice(0, 5)) > -1 ? setImmediate : pna.nextTick;
/*</replacement>*/

/*<replacement>*/
var Duplex;
/*</replacement>*/

Writable.WritableState = WritableState;

/*<replacement>*/
var util = Object.create(require('core-util-is'));
util.inherits = require('inherits');
/*</replacement>*/

/*<replacement>*/
var internalUtil = {
  deprecate: require('util-deprecate')
};
/*</replacement>*/

/*<replacement>*/
var Stream = require('./internal/streams/stream');
/*</replacement>*/

/*<replacement>*/

var Buffer = require('safe-buffer').Buffer;
var OurUint8Array = global.Uint8Array || function () {};
function _uint8ArrayToBuffer(chunk) {
  return Buffer.from(chunk);
}
function _isUint8Array(obj) {
  return Buffer.isBuffer(obj) || obj instanceof OurUint8Array;
}

/*</replacement>*/

var destroyImpl = require('./internal/streams/destroy');

util.inherits(Writable, Stream);

function nop() {}

function WritableState(options, stream) {
  Duplex = Duplex || require('./_stream_duplex');

  options = options || {};

  // Duplex streams are both readable and writable, but share
  // the same options object.
  // However, some cases require setting options to different
  // values for the readable and the writable sides of the duplex stream.
  // These options can be provided separately as readableXXX and writableXXX.
  var isDuplex = stream instanceof Duplex;

  // object stream flag to indicate whether or not this stream
  // contains buffers or objects.
  this.objectMode = !!options.objectMode;

  if (isDuplex) this.objectMode = this.objectMode || !!options.writableObjectMode;

  // the point at which write() starts returning false
  // Note: 0 is a valid value, means that we always return false if
  // the entire buffer is not flushed immediately on write()
  var hwm = options.highWaterMark;
  var writableHwm = options.writableHighWaterMark;
  var defaultHwm = this.objectMode ? 16 : 16 * 1024;

  if (hwm || hwm === 0) this.highWaterMark = hwm;else if (isDuplex && (writableHwm || writableHwm === 0)) this.highWaterMark = writableHwm;else this.highWaterMark = defaultHwm;

  // cast to ints.
  this.highWaterMark = Math.floor(this.highWaterMark);

  // if _final has been called
  this.finalCalled = false;

  // drain event flag.
  this.needDrain = false;
  // at the start of calling end()
  this.ending = false;
  // when end() has been called, and returned
  this.ended = false;
  // when 'finish' is emitted
  this.finished = false;

  // has it been destroyed
  this.destroyed = false;

  // should we decode strings into buffers before passing to _write?
  // this is here so that some node-core streams can optimize string
  // handling at a lower level.
  var noDecode = options.decodeStrings === false;
  this.decodeStrings = !noDecode;

  // Crypto is kind of old and crusty.  Historically, its default string
  // encoding is 'binary' so we have to make this configurable.
  // Everything else in the universe uses 'utf8', though.
  this.defaultEncoding = options.defaultEncoding || 'utf8';

  // not an actual buffer we keep track of, but a measurement
  // of how much we're waiting to get pushed to some underlying
  // socket or file.
  this.length = 0;

  // a flag to see when we're in the middle of a write.
  this.writing = false;

  // when true all writes will be buffered until .uncork() call
  this.corked = 0;

  // a flag to be able to tell if the onwrite cb is called immediately,
  // or on a later tick.  We set this to true at first, because any
  // actions that shouldn't happen until "later" should generally also
  // not happen before the first write call.
  this.sync = true;

  // a flag to know if we're processing previously buffered items, which
  // may call the _write() callback in the same tick, so that we don't
  // end up in an overlapped onwrite situation.
  this.bufferProcessing = false;

  // the callback that's passed to _write(chunk,cb)
  this.onwrite = function (er) {
    onwrite(stream, er);
  };

  // the callback that the user supplies to write(chunk,encoding,cb)
  this.writecb = null;

  // the amount that is being written when _write is called.
  this.writelen = 0;

  this.bufferedRequest = null;
  this.lastBufferedRequest = null;

  // number of pending user-supplied write callbacks
  // this must be 0 before 'finish' can be emitted
  this.pendingcb = 0;

  // emit prefinish if the only thing we're waiting for is _write cbs
  // This is relevant for synchronous Transform streams
  this.prefinished = false;

  // True if the error was already emitted and should not be thrown again
  this.errorEmitted = false;

  // count buffered requests
  this.bufferedRequestCount = 0;

  // allocate the first CorkedRequest, there is always
  // one allocated and free to use, and we maintain at most two
  this.corkedRequestsFree = new CorkedRequest(this);
}

WritableState.prototype.getBuffer = function getBuffer() {
  var current = this.bufferedRequest;
  var out = [];
  while (current) {
    out.push(current);
    current = current.next;
  }
  return out;
};

(function () {
  try {
    Object.defineProperty(WritableState.prototype, 'buffer', {
      get: internalUtil.deprecate(function () {
        return this.getBuffer();
      }, '_writableState.buffer is deprecated. Use _writableState.getBuffer ' + 'instead.', 'DEP0003')
    });
  } catch (_) {}
})();

// Test _writableState for inheritance to account for Duplex streams,
// whose prototype chain only points to Readable.
var realHasInstance;
if (typeof Symbol === 'function' && Symbol.hasInstance && typeof Function.prototype[Symbol.hasInstance] === 'function') {
  realHasInstance = Function.prototype[Symbol.hasInstance];
  Object.defineProperty(Writable, Symbol.hasInstance, {
    value: function (object) {
      if (realHasInstance.call(this, object)) return true;
      if (this !== Writable) return false;

      return object && object._writableState instanceof WritableState;
    }
  });
} else {
  realHasInstance = function (object) {
    return object instanceof this;
  };
}

function Writable(options) {
  Duplex = Duplex || require('./_stream_duplex');

  // Writable ctor is applied to Duplexes, too.
  // `realHasInstance` is necessary because using plain `instanceof`
  // would return false, as no `_writableState` property is attached.

  // Trying to use the custom `instanceof` for Writable here will also break the
  // Node.js LazyTransform implementation, which has a non-trivial getter for
  // `_writableState` that would lead to infinite recursion.
  if (!realHasInstance.call(Writable, this) && !(this instanceof Duplex)) {
    return new Writable(options);
  }

  this._writableState = new WritableState(options, this);

  // legacy.
  this.writable = true;

  if (options) {
    if (typeof options.write === 'function') this._write = options.write;

    if (typeof options.writev === 'function') this._writev = options.writev;

    if (typeof options.destroy === 'function') this._destroy = options.destroy;

    if (typeof options.final === 'function') this._final = options.final;
  }

  Stream.call(this);
}

// Otherwise people can pipe Writable streams, which is just wrong.
Writable.prototype.pipe = function () {
  this.emit('error', new Error('Cannot pipe, not readable'));
};

function writeAfterEnd(stream, cb) {
  var er = new Error('write after end');
  // TODO: defer error events consistently everywhere, not just the cb
  stream.emit('error', er);
  pna.nextTick(cb, er);
}

// Checks that a user-supplied chunk is valid, especially for the particular
// mode the stream is in. Currently this means that `null` is never accepted
// and undefined/non-string values are only allowed in object mode.
function validChunk(stream, state, chunk, cb) {
  var valid = true;
  var er = false;

  if (chunk === null) {
    er = new TypeError('May not write null values to stream');
  } else if (typeof chunk !== 'string' && chunk !== undefined && !state.objectMode) {
    er = new TypeError('Invalid non-string/buffer chunk');
  }
  if (er) {
    stream.emit('error', er);
    pna.nextTick(cb, er);
    valid = false;
  }
  return valid;
}

Writable.prototype.write = function (chunk, encoding, cb) {
  var state = this._writableState;
  var ret = false;
  var isBuf = !state.objectMode && _isUint8Array(chunk);

  if (isBuf && !Buffer.isBuffer(chunk)) {
    chunk = _uint8ArrayToBuffer(chunk);
  }

  if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (isBuf) encoding = 'buffer';else if (!encoding) encoding = state.defaultEncoding;

  if (typeof cb !== 'function') cb = nop;

  if (state.ended) writeAfterEnd(this, cb);else if (isBuf || validChunk(this, state, chunk, cb)) {
    state.pendingcb++;
    ret = writeOrBuffer(this, state, isBuf, chunk, encoding, cb);
  }

  return ret;
};

Writable.prototype.cork = function () {
  var state = this._writableState;

  state.corked++;
};

Writable.prototype.uncork = function () {
  var state = this._writableState;

  if (state.corked) {
    state.corked--;

    if (!state.writing && !state.corked && !state.finished && !state.bufferProcessing && state.bufferedRequest) clearBuffer(this, state);
  }
};

Writable.prototype.setDefaultEncoding = function setDefaultEncoding(encoding) {
  // node::ParseEncoding() requires lower case.
  if (typeof encoding === 'string') encoding = encoding.toLowerCase();
  if (!(['hex', 'utf8', 'utf-8', 'ascii', 'binary', 'base64', 'ucs2', 'ucs-2', 'utf16le', 'utf-16le', 'raw'].indexOf((encoding + '').toLowerCase()) > -1)) throw new TypeError('Unknown encoding: ' + encoding);
  this._writableState.defaultEncoding = encoding;
  return this;
};

function decodeChunk(state, chunk, encoding) {
  if (!state.objectMode && state.decodeStrings !== false && typeof chunk === 'string') {
    chunk = Buffer.from(chunk, encoding);
  }
  return chunk;
}

Object.defineProperty(Writable.prototype, 'writableHighWaterMark', {
  // making it explicit this property is not enumerable
  // because otherwise some prototype manipulation in
  // userland will fail
  enumerable: false,
  get: function () {
    return this._writableState.highWaterMark;
  }
});

// if we're already writing something, then just put this
// in the queue, and wait our turn.  Otherwise, call _write
// If we return false, then we need a drain event, so set that flag.
function writeOrBuffer(stream, state, isBuf, chunk, encoding, cb) {
  if (!isBuf) {
    var newChunk = decodeChunk(state, chunk, encoding);
    if (chunk !== newChunk) {
      isBuf = true;
      encoding = 'buffer';
      chunk = newChunk;
    }
  }
  var len = state.objectMode ? 1 : chunk.length;

  state.length += len;

  var ret = state.length < state.highWaterMark;
  // we must ensure that previous needDrain will not be reset to false.
  if (!ret) state.needDrain = true;

  if (state.writing || state.corked) {
    var last = state.lastBufferedRequest;
    state.lastBufferedRequest = {
      chunk: chunk,
      encoding: encoding,
      isBuf: isBuf,
      callback: cb,
      next: null
    };
    if (last) {
      last.next = state.lastBufferedRequest;
    } else {
      state.bufferedRequest = state.lastBufferedRequest;
    }
    state.bufferedRequestCount += 1;
  } else {
    doWrite(stream, state, false, len, chunk, encoding, cb);
  }

  return ret;
}

function doWrite(stream, state, writev, len, chunk, encoding, cb) {
  state.writelen = len;
  state.writecb = cb;
  state.writing = true;
  state.sync = true;
  if (writev) stream._writev(chunk, state.onwrite);else stream._write(chunk, encoding, state.onwrite);
  state.sync = false;
}

function onwriteError(stream, state, sync, er, cb) {
  --state.pendingcb;

  if (sync) {
    // defer the callback if we are being called synchronously
    // to avoid piling up things on the stack
    pna.nextTick(cb, er);
    // this can emit finish, and it will always happen
    // after error
    pna.nextTick(finishMaybe, stream, state);
    stream._writableState.errorEmitted = true;
    stream.emit('error', er);
  } else {
    // the caller expect this to happen before if
    // it is async
    cb(er);
    stream._writableState.errorEmitted = true;
    stream.emit('error', er);
    // this can emit finish, but finish must
    // always follow error
    finishMaybe(stream, state);
  }
}

function onwriteStateUpdate(state) {
  state.writing = false;
  state.writecb = null;
  state.length -= state.writelen;
  state.writelen = 0;
}

function onwrite(stream, er) {
  var state = stream._writableState;
  var sync = state.sync;
  var cb = state.writecb;

  onwriteStateUpdate(state);

  if (er) onwriteError(stream, state, sync, er, cb);else {
    // Check if we're actually ready to finish, but don't emit yet
    var finished = needFinish(state);

    if (!finished && !state.corked && !state.bufferProcessing && state.bufferedRequest) {
      clearBuffer(stream, state);
    }

    if (sync) {
      /*<replacement>*/
      asyncWrite(afterWrite, stream, state, finished, cb);
      /*</replacement>*/
    } else {
      afterWrite(stream, state, finished, cb);
    }
  }
}

function afterWrite(stream, state, finished, cb) {
  if (!finished) onwriteDrain(stream, state);
  state.pendingcb--;
  cb();
  finishMaybe(stream, state);
}

// Must force callback to be called on nextTick, so that we don't
// emit 'drain' before the write() consumer gets the 'false' return
// value, and has a chance to attach a 'drain' listener.
function onwriteDrain(stream, state) {
  if (state.length === 0 && state.needDrain) {
    state.needDrain = false;
    stream.emit('drain');
  }
}

// if there's something in the buffer waiting, then process it
function clearBuffer(stream, state) {
  state.bufferProcessing = true;
  var entry = state.bufferedRequest;

  if (stream._writev && entry && entry.next) {
    // Fast case, write everything using _writev()
    var l = state.bufferedRequestCount;
    var buffer = new Array(l);
    var holder = state.corkedRequestsFree;
    holder.entry = entry;

    var count = 0;
    var allBuffers = true;
    while (entry) {
      buffer[count] = entry;
      if (!entry.isBuf) allBuffers = false;
      entry = entry.next;
      count += 1;
    }
    buffer.allBuffers = allBuffers;

    doWrite(stream, state, true, state.length, buffer, '', holder.finish);

    // doWrite is almost always async, defer these to save a bit of time
    // as the hot path ends with doWrite
    state.pendingcb++;
    state.lastBufferedRequest = null;
    if (holder.next) {
      state.corkedRequestsFree = holder.next;
      holder.next = null;
    } else {
      state.corkedRequestsFree = new CorkedRequest(state);
    }
    state.bufferedRequestCount = 0;
  } else {
    // Slow case, write chunks one-by-one
    while (entry) {
      var chunk = entry.chunk;
      var encoding = entry.encoding;
      var cb = entry.callback;
      var len = state.objectMode ? 1 : chunk.length;

      doWrite(stream, state, false, len, chunk, encoding, cb);
      entry = entry.next;
      state.bufferedRequestCount--;
      // if we didn't call the onwrite immediately, then
      // it means that we need to wait until it does.
      // also, that means that the chunk and cb are currently
      // being processed, so move the buffer counter past them.
      if (state.writing) {
        break;
      }
    }

    if (entry === null) state.lastBufferedRequest = null;
  }

  state.bufferedRequest = entry;
  state.bufferProcessing = false;
}

Writable.prototype._write = function (chunk, encoding, cb) {
  cb(new Error('_write() is not implemented'));
};

Writable.prototype._writev = null;

Writable.prototype.end = function (chunk, encoding, cb) {
  var state = this._writableState;

  if (typeof chunk === 'function') {
    cb = chunk;
    chunk = null;
    encoding = null;
  } else if (typeof encoding === 'function') {
    cb = encoding;
    encoding = null;
  }

  if (chunk !== null && chunk !== undefined) this.write(chunk, encoding);

  // .end() fully uncorks
  if (state.corked) {
    state.corked = 1;
    this.uncork();
  }

  // ignore unnecessary end() calls.
  if (!state.ending && !state.finished) endWritable(this, state, cb);
};

function needFinish(state) {
  return state.ending && state.length === 0 && state.bufferedRequest === null && !state.finished && !state.writing;
}
function callFinal(stream, state) {
  stream._final(function (err) {
    state.pendingcb--;
    if (err) {
      stream.emit('error', err);
    }
    state.prefinished = true;
    stream.emit('prefinish');
    finishMaybe(stream, state);
  });
}
function prefinish(stream, state) {
  if (!state.prefinished && !state.finalCalled) {
    if (typeof stream._final === 'function') {
      state.pendingcb++;
      state.finalCalled = true;
      pna.nextTick(callFinal, stream, state);
    } else {
      state.prefinished = true;
      stream.emit('prefinish');
    }
  }
}

function finishMaybe(stream, state) {
  var need = needFinish(state);
  if (need) {
    prefinish(stream, state);
    if (state.pendingcb === 0) {
      state.finished = true;
      stream.emit('finish');
    }
  }
  return need;
}

function endWritable(stream, state, cb) {
  state.ending = true;
  finishMaybe(stream, state);
  if (cb) {
    if (state.finished) pna.nextTick(cb);else stream.once('finish', cb);
  }
  state.ended = true;
  stream.writable = false;
}

function onCorkedFinish(corkReq, state, err) {
  var entry = corkReq.entry;
  corkReq.entry = null;
  while (entry) {
    var cb = entry.callback;
    state.pendingcb--;
    cb(err);
    entry = entry.next;
  }
  if (state.corkedRequestsFree) {
    state.corkedRequestsFree.next = corkReq;
  } else {
    state.corkedRequestsFree = corkReq;
  }
}

Object.defineProperty(Writable.prototype, 'destroyed', {
  get: function () {
    if (this._writableState === undefined) {
      return false;
    }
    return this._writableState.destroyed;
  },
  set: function (value) {
    // we ignore the value if the stream
    // has not been initialized yet
    if (!this._writableState) {
      return;
    }

    // backward compatibility, the user is explicitly
    // managing destroyed
    this._writableState.destroyed = value;
  }
});

Writable.prototype.destroy = destroyImpl.destroy;
Writable.prototype._undestroy = destroyImpl.undestroy;
Writable.prototype._destroy = function (err, cb) {
  this.end();
  cb(err);
};
}).call(this,require('_process'),typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {},require("timers").setImmediate)
},{"./_stream_duplex":48,"./internal/streams/destroy":54,"./internal/streams/stream":55,"_process":46,"core-util-is":38,"inherits":41,"process-nextick-args":45,"safe-buffer":56,"timers":65,"util-deprecate":66}],53:[function(require,module,exports){
'use strict';

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Buffer = require('safe-buffer').Buffer;
var util = require('util');

function copyBuffer(src, target, offset) {
  src.copy(target, offset);
}

module.exports = function () {
  function BufferList() {
    _classCallCheck(this, BufferList);

    this.head = null;
    this.tail = null;
    this.length = 0;
  }

  BufferList.prototype.push = function push(v) {
    var entry = { data: v, next: null };
    if (this.length > 0) this.tail.next = entry;else this.head = entry;
    this.tail = entry;
    ++this.length;
  };

  BufferList.prototype.unshift = function unshift(v) {
    var entry = { data: v, next: this.head };
    if (this.length === 0) this.tail = entry;
    this.head = entry;
    ++this.length;
  };

  BufferList.prototype.shift = function shift() {
    if (this.length === 0) return;
    var ret = this.head.data;
    if (this.length === 1) this.head = this.tail = null;else this.head = this.head.next;
    --this.length;
    return ret;
  };

  BufferList.prototype.clear = function clear() {
    this.head = this.tail = null;
    this.length = 0;
  };

  BufferList.prototype.join = function join(s) {
    if (this.length === 0) return '';
    var p = this.head;
    var ret = '' + p.data;
    while (p = p.next) {
      ret += s + p.data;
    }return ret;
  };

  BufferList.prototype.concat = function concat(n) {
    if (this.length === 0) return Buffer.alloc(0);
    if (this.length === 1) return this.head.data;
    var ret = Buffer.allocUnsafe(n >>> 0);
    var p = this.head;
    var i = 0;
    while (p) {
      copyBuffer(p.data, ret, i);
      i += p.data.length;
      p = p.next;
    }
    return ret;
  };

  return BufferList;
}();

if (util && util.inspect && util.inspect.custom) {
  module.exports.prototype[util.inspect.custom] = function () {
    var obj = util.inspect({ length: this.length });
    return this.constructor.name + ' ' + obj;
  };
}
},{"safe-buffer":56,"util":36}],54:[function(require,module,exports){
'use strict';

/*<replacement>*/

var pna = require('process-nextick-args');
/*</replacement>*/

// undocumented cb() API, needed for core, not for public API
function destroy(err, cb) {
  var _this = this;

  var readableDestroyed = this._readableState && this._readableState.destroyed;
  var writableDestroyed = this._writableState && this._writableState.destroyed;

  if (readableDestroyed || writableDestroyed) {
    if (cb) {
      cb(err);
    } else if (err && (!this._writableState || !this._writableState.errorEmitted)) {
      pna.nextTick(emitErrorNT, this, err);
    }
    return this;
  }

  // we set destroyed to true before firing error callbacks in order
  // to make it re-entrance safe in case destroy() is called within callbacks

  if (this._readableState) {
    this._readableState.destroyed = true;
  }

  // if this is a duplex stream mark the writable part as destroyed as well
  if (this._writableState) {
    this._writableState.destroyed = true;
  }

  this._destroy(err || null, function (err) {
    if (!cb && err) {
      pna.nextTick(emitErrorNT, _this, err);
      if (_this._writableState) {
        _this._writableState.errorEmitted = true;
      }
    } else if (cb) {
      cb(err);
    }
  });

  return this;
}

function undestroy() {
  if (this._readableState) {
    this._readableState.destroyed = false;
    this._readableState.reading = false;
    this._readableState.ended = false;
    this._readableState.endEmitted = false;
  }

  if (this._writableState) {
    this._writableState.destroyed = false;
    this._writableState.ended = false;
    this._writableState.ending = false;
    this._writableState.finished = false;
    this._writableState.errorEmitted = false;
  }
}

function emitErrorNT(self, err) {
  self.emit('error', err);
}

module.exports = {
  destroy: destroy,
  undestroy: undestroy
};
},{"process-nextick-args":45}],55:[function(require,module,exports){
module.exports = require('events').EventEmitter;

},{"events":39}],56:[function(require,module,exports){
arguments[4][23][0].apply(exports,arguments)
},{"buffer":37,"dup":23}],57:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

'use strict';

/*<replacement>*/

var Buffer = require('safe-buffer').Buffer;
/*</replacement>*/

var isEncoding = Buffer.isEncoding || function (encoding) {
  encoding = '' + encoding;
  switch (encoding && encoding.toLowerCase()) {
    case 'hex':case 'utf8':case 'utf-8':case 'ascii':case 'binary':case 'base64':case 'ucs2':case 'ucs-2':case 'utf16le':case 'utf-16le':case 'raw':
      return true;
    default:
      return false;
  }
};

function _normalizeEncoding(enc) {
  if (!enc) return 'utf8';
  var retried;
  while (true) {
    switch (enc) {
      case 'utf8':
      case 'utf-8':
        return 'utf8';
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return 'utf16le';
      case 'latin1':
      case 'binary':
        return 'latin1';
      case 'base64':
      case 'ascii':
      case 'hex':
        return enc;
      default:
        if (retried) return; // undefined
        enc = ('' + enc).toLowerCase();
        retried = true;
    }
  }
};

// Do not cache `Buffer.isEncoding` when checking encoding names as some
// modules monkey-patch it to support additional encodings
function normalizeEncoding(enc) {
  var nenc = _normalizeEncoding(enc);
  if (typeof nenc !== 'string' && (Buffer.isEncoding === isEncoding || !isEncoding(enc))) throw new Error('Unknown encoding: ' + enc);
  return nenc || enc;
}

// StringDecoder provides an interface for efficiently splitting a series of
// buffers into a series of JS strings without breaking apart multi-byte
// characters.
exports.StringDecoder = StringDecoder;
function StringDecoder(encoding) {
  this.encoding = normalizeEncoding(encoding);
  var nb;
  switch (this.encoding) {
    case 'utf16le':
      this.text = utf16Text;
      this.end = utf16End;
      nb = 4;
      break;
    case 'utf8':
      this.fillLast = utf8FillLast;
      nb = 4;
      break;
    case 'base64':
      this.text = base64Text;
      this.end = base64End;
      nb = 3;
      break;
    default:
      this.write = simpleWrite;
      this.end = simpleEnd;
      return;
  }
  this.lastNeed = 0;
  this.lastTotal = 0;
  this.lastChar = Buffer.allocUnsafe(nb);
}

StringDecoder.prototype.write = function (buf) {
  if (buf.length === 0) return '';
  var r;
  var i;
  if (this.lastNeed) {
    r = this.fillLast(buf);
    if (r === undefined) return '';
    i = this.lastNeed;
    this.lastNeed = 0;
  } else {
    i = 0;
  }
  if (i < buf.length) return r ? r + this.text(buf, i) : this.text(buf, i);
  return r || '';
};

StringDecoder.prototype.end = utf8End;

// Returns only complete characters in a Buffer
StringDecoder.prototype.text = utf8Text;

// Attempts to complete a partial non-UTF-8 character using bytes from a Buffer
StringDecoder.prototype.fillLast = function (buf) {
  if (this.lastNeed <= buf.length) {
    buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, this.lastNeed);
    return this.lastChar.toString(this.encoding, 0, this.lastTotal);
  }
  buf.copy(this.lastChar, this.lastTotal - this.lastNeed, 0, buf.length);
  this.lastNeed -= buf.length;
};

// Checks the type of a UTF-8 byte, whether it's ASCII, a leading byte, or a
// continuation byte. If an invalid byte is detected, -2 is returned.
function utf8CheckByte(byte) {
  if (byte <= 0x7F) return 0;else if (byte >> 5 === 0x06) return 2;else if (byte >> 4 === 0x0E) return 3;else if (byte >> 3 === 0x1E) return 4;
  return byte >> 6 === 0x02 ? -1 : -2;
}

// Checks at most 3 bytes at the end of a Buffer in order to detect an
// incomplete multi-byte UTF-8 character. The total number of bytes (2, 3, or 4)
// needed to complete the UTF-8 character (if applicable) are returned.
function utf8CheckIncomplete(self, buf, i) {
  var j = buf.length - 1;
  if (j < i) return 0;
  var nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) self.lastNeed = nb - 1;
    return nb;
  }
  if (--j < i || nb === -2) return 0;
  nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) self.lastNeed = nb - 2;
    return nb;
  }
  if (--j < i || nb === -2) return 0;
  nb = utf8CheckByte(buf[j]);
  if (nb >= 0) {
    if (nb > 0) {
      if (nb === 2) nb = 0;else self.lastNeed = nb - 3;
    }
    return nb;
  }
  return 0;
}

// Validates as many continuation bytes for a multi-byte UTF-8 character as
// needed or are available. If we see a non-continuation byte where we expect
// one, we "replace" the validated continuation bytes we've seen so far with
// a single UTF-8 replacement character ('\ufffd'), to match v8's UTF-8 decoding
// behavior. The continuation byte check is included three times in the case
// where all of the continuation bytes for a character exist in the same buffer.
// It is also done this way as a slight performance increase instead of using a
// loop.
function utf8CheckExtraBytes(self, buf, p) {
  if ((buf[0] & 0xC0) !== 0x80) {
    self.lastNeed = 0;
    return '\ufffd';
  }
  if (self.lastNeed > 1 && buf.length > 1) {
    if ((buf[1] & 0xC0) !== 0x80) {
      self.lastNeed = 1;
      return '\ufffd';
    }
    if (self.lastNeed > 2 && buf.length > 2) {
      if ((buf[2] & 0xC0) !== 0x80) {
        self.lastNeed = 2;
        return '\ufffd';
      }
    }
  }
}

// Attempts to complete a multi-byte UTF-8 character using bytes from a Buffer.
function utf8FillLast(buf) {
  var p = this.lastTotal - this.lastNeed;
  var r = utf8CheckExtraBytes(this, buf, p);
  if (r !== undefined) return r;
  if (this.lastNeed <= buf.length) {
    buf.copy(this.lastChar, p, 0, this.lastNeed);
    return this.lastChar.toString(this.encoding, 0, this.lastTotal);
  }
  buf.copy(this.lastChar, p, 0, buf.length);
  this.lastNeed -= buf.length;
}

// Returns all complete UTF-8 characters in a Buffer. If the Buffer ended on a
// partial character, the character's bytes are buffered until the required
// number of bytes are available.
function utf8Text(buf, i) {
  var total = utf8CheckIncomplete(this, buf, i);
  if (!this.lastNeed) return buf.toString('utf8', i);
  this.lastTotal = total;
  var end = buf.length - (total - this.lastNeed);
  buf.copy(this.lastChar, 0, end);
  return buf.toString('utf8', i, end);
}

// For UTF-8, a replacement character is added when ending on a partial
// character.
function utf8End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) return r + '\ufffd';
  return r;
}

// UTF-16LE typically needs two bytes per character, but even if we have an even
// number of bytes available, we need to check if we end on a leading/high
// surrogate. In that case, we need to wait for the next two bytes in order to
// decode the last character properly.
function utf16Text(buf, i) {
  if ((buf.length - i) % 2 === 0) {
    var r = buf.toString('utf16le', i);
    if (r) {
      var c = r.charCodeAt(r.length - 1);
      if (c >= 0xD800 && c <= 0xDBFF) {
        this.lastNeed = 2;
        this.lastTotal = 4;
        this.lastChar[0] = buf[buf.length - 2];
        this.lastChar[1] = buf[buf.length - 1];
        return r.slice(0, -1);
      }
    }
    return r;
  }
  this.lastNeed = 1;
  this.lastTotal = 2;
  this.lastChar[0] = buf[buf.length - 1];
  return buf.toString('utf16le', i, buf.length - 1);
}

// For UTF-16LE we do not explicitly append special replacement characters if we
// end on a partial character, we simply let v8 handle that.
function utf16End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) {
    var end = this.lastTotal - this.lastNeed;
    return r + this.lastChar.toString('utf16le', 0, end);
  }
  return r;
}

function base64Text(buf, i) {
  var n = (buf.length - i) % 3;
  if (n === 0) return buf.toString('base64', i);
  this.lastNeed = 3 - n;
  this.lastTotal = 3;
  if (n === 1) {
    this.lastChar[0] = buf[buf.length - 1];
  } else {
    this.lastChar[0] = buf[buf.length - 2];
    this.lastChar[1] = buf[buf.length - 1];
  }
  return buf.toString('base64', i, buf.length - n);
}

function base64End(buf) {
  var r = buf && buf.length ? this.write(buf) : '';
  if (this.lastNeed) return r + this.lastChar.toString('base64', 0, 3 - this.lastNeed);
  return r;
}

// Pass bytes on through for single-byte encodings (e.g. ascii, latin1, hex)
function simpleWrite(buf) {
  return buf.toString(this.encoding);
}

function simpleEnd(buf) {
  return buf && buf.length ? this.write(buf) : '';
}
},{"safe-buffer":56}],58:[function(require,module,exports){
module.exports = require('./readable').PassThrough

},{"./readable":59}],59:[function(require,module,exports){
exports = module.exports = require('./lib/_stream_readable.js');
exports.Stream = exports;
exports.Readable = exports;
exports.Writable = require('./lib/_stream_writable.js');
exports.Duplex = require('./lib/_stream_duplex.js');
exports.Transform = require('./lib/_stream_transform.js');
exports.PassThrough = require('./lib/_stream_passthrough.js');

},{"./lib/_stream_duplex.js":48,"./lib/_stream_passthrough.js":49,"./lib/_stream_readable.js":50,"./lib/_stream_transform.js":51,"./lib/_stream_writable.js":52}],60:[function(require,module,exports){
module.exports = require('./readable').Transform

},{"./readable":59}],61:[function(require,module,exports){
module.exports = require('./lib/_stream_writable.js');

},{"./lib/_stream_writable.js":52}],62:[function(require,module,exports){
/*! safe-buffer. MIT License. Feross Aboukhadijeh <https://feross.org/opensource> */
/* eslint-disable node/no-deprecated-api */
var buffer = require('buffer')
var Buffer = buffer.Buffer

// alternative to using Object.keys for old browsers
function copyProps (src, dst) {
  for (var key in src) {
    dst[key] = src[key]
  }
}
if (Buffer.from && Buffer.alloc && Buffer.allocUnsafe && Buffer.allocUnsafeSlow) {
  module.exports = buffer
} else {
  // Copy properties from require('buffer')
  copyProps(buffer, exports)
  exports.Buffer = SafeBuffer
}

function SafeBuffer (arg, encodingOrOffset, length) {
  return Buffer(arg, encodingOrOffset, length)
}

SafeBuffer.prototype = Object.create(Buffer.prototype)

// Copy static methods from Buffer
copyProps(Buffer, SafeBuffer)

SafeBuffer.from = function (arg, encodingOrOffset, length) {
  if (typeof arg === 'number') {
    throw new TypeError('Argument must not be a number')
  }
  return Buffer(arg, encodingOrOffset, length)
}

SafeBuffer.alloc = function (size, fill, encoding) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  var buf = Buffer(size)
  if (fill !== undefined) {
    if (typeof encoding === 'string') {
      buf.fill(fill, encoding)
    } else {
      buf.fill(fill)
    }
  } else {
    buf.fill(0)
  }
  return buf
}

SafeBuffer.allocUnsafe = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return Buffer(size)
}

SafeBuffer.allocUnsafeSlow = function (size) {
  if (typeof size !== 'number') {
    throw new TypeError('Argument must be a number')
  }
  return buffer.SlowBuffer(size)
}

},{"buffer":37}],63:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

module.exports = Stream;

var EE = require('events').EventEmitter;
var inherits = require('inherits');

inherits(Stream, EE);
Stream.Readable = require('readable-stream/readable.js');
Stream.Writable = require('readable-stream/writable.js');
Stream.Duplex = require('readable-stream/duplex.js');
Stream.Transform = require('readable-stream/transform.js');
Stream.PassThrough = require('readable-stream/passthrough.js');

// Backwards-compat with node 0.4.x
Stream.Stream = Stream;



// old-style streams.  Note that the pipe method (the only relevant
// part of this class) is overridden in the Readable class.

function Stream() {
  EE.call(this);
}

Stream.prototype.pipe = function(dest, options) {
  var source = this;

  function ondata(chunk) {
    if (dest.writable) {
      if (false === dest.write(chunk) && source.pause) {
        source.pause();
      }
    }
  }

  source.on('data', ondata);

  function ondrain() {
    if (source.readable && source.resume) {
      source.resume();
    }
  }

  dest.on('drain', ondrain);

  // If the 'end' option is not supplied, dest.end() will be called when
  // source gets the 'end' or 'close' events.  Only dest.end() once.
  if (!dest._isStdio && (!options || options.end !== false)) {
    source.on('end', onend);
    source.on('close', onclose);
  }

  var didOnEnd = false;
  function onend() {
    if (didOnEnd) return;
    didOnEnd = true;

    dest.end();
  }


  function onclose() {
    if (didOnEnd) return;
    didOnEnd = true;

    if (typeof dest.destroy === 'function') dest.destroy();
  }

  // don't leave dangling pipes when there are errors.
  function onerror(er) {
    cleanup();
    if (EE.listenerCount(this, 'error') === 0) {
      throw er; // Unhandled stream error in pipe.
    }
  }

  source.on('error', onerror);
  dest.on('error', onerror);

  // remove all the event listeners that were added.
  function cleanup() {
    source.removeListener('data', ondata);
    dest.removeListener('drain', ondrain);

    source.removeListener('end', onend);
    source.removeListener('close', onclose);

    source.removeListener('error', onerror);
    dest.removeListener('error', onerror);

    source.removeListener('end', cleanup);
    source.removeListener('close', cleanup);

    dest.removeListener('close', cleanup);
  }

  source.on('end', cleanup);
  source.on('close', cleanup);

  dest.on('close', cleanup);

  dest.emit('pipe', source);

  // Allow for unix-like usage: A.pipe(B).pipe(C)
  return dest;
};

},{"events":39,"inherits":41,"readable-stream/duplex.js":47,"readable-stream/passthrough.js":58,"readable-stream/readable.js":59,"readable-stream/transform.js":60,"readable-stream/writable.js":61}],64:[function(require,module,exports){
arguments[4][57][0].apply(exports,arguments)
},{"dup":57,"safe-buffer":62}],65:[function(require,module,exports){
(function (setImmediate,clearImmediate){
var nextTick = require('process/browser.js').nextTick;
var apply = Function.prototype.apply;
var slice = Array.prototype.slice;
var immediateIds = {};
var nextImmediateId = 0;

// DOM APIs, for completeness

exports.setTimeout = function() {
  return new Timeout(apply.call(setTimeout, window, arguments), clearTimeout);
};
exports.setInterval = function() {
  return new Timeout(apply.call(setInterval, window, arguments), clearInterval);
};
exports.clearTimeout =
exports.clearInterval = function(timeout) { timeout.close(); };

function Timeout(id, clearFn) {
  this._id = id;
  this._clearFn = clearFn;
}
Timeout.prototype.unref = Timeout.prototype.ref = function() {};
Timeout.prototype.close = function() {
  this._clearFn.call(window, this._id);
};

// Does not start the time, just sets up the members needed.
exports.enroll = function(item, msecs) {
  clearTimeout(item._idleTimeoutId);
  item._idleTimeout = msecs;
};

exports.unenroll = function(item) {
  clearTimeout(item._idleTimeoutId);
  item._idleTimeout = -1;
};

exports._unrefActive = exports.active = function(item) {
  clearTimeout(item._idleTimeoutId);

  var msecs = item._idleTimeout;
  if (msecs >= 0) {
    item._idleTimeoutId = setTimeout(function onTimeout() {
      if (item._onTimeout)
        item._onTimeout();
    }, msecs);
  }
};

// That's not how node.js implements it but the exposed api is the same.
exports.setImmediate = typeof setImmediate === "function" ? setImmediate : function(fn) {
  var id = nextImmediateId++;
  var args = arguments.length < 2 ? false : slice.call(arguments, 1);

  immediateIds[id] = true;

  nextTick(function onNextTick() {
    if (immediateIds[id]) {
      // fn.call() is faster so we optimize for the common use-case
      // @see http://jsperf.com/call-apply-segu
      if (args) {
        fn.apply(null, args);
      } else {
        fn.call(null);
      }
      // Prevent ids from leaking
      exports.clearImmediate(id);
    }
  });

  return id;
};

exports.clearImmediate = typeof clearImmediate === "function" ? clearImmediate : function(id) {
  delete immediateIds[id];
};
}).call(this,require("timers").setImmediate,require("timers").clearImmediate)
},{"process/browser.js":46,"timers":65}],66:[function(require,module,exports){
(function (global){

/**
 * Module exports.
 */

module.exports = deprecate;

/**
 * Mark that a method should not be used.
 * Returns a modified function which warns once by default.
 *
 * If `localStorage.noDeprecation = true` is set, then it is a no-op.
 *
 * If `localStorage.throwDeprecation = true` is set, then deprecated functions
 * will throw an Error when invoked.
 *
 * If `localStorage.traceDeprecation = true` is set, then deprecated functions
 * will invoke `console.trace()` instead of `console.error()`.
 *
 * @param {Function} fn - the function to deprecate
 * @param {String} msg - the string to print to the console when `fn` is invoked
 * @returns {Function} a new "deprecated" version of `fn`
 * @api public
 */

function deprecate (fn, msg) {
  if (config('noDeprecation')) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (config('throwDeprecation')) {
        throw new Error(msg);
      } else if (config('traceDeprecation')) {
        console.trace(msg);
      } else {
        console.warn(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
}

/**
 * Checks `localStorage` for boolean values for the given `name`.
 *
 * @param {String} name
 * @returns {Boolean}
 * @api private
 */

function config (name) {
  // accessing global.localStorage can trigger a DOMException in sandboxed iframes
  try {
    if (!global.localStorage) return false;
  } catch (_) {
    return false;
  }
  var val = global.localStorage[name];
  if (null == val) return false;
  return String(val).toLowerCase() === 'true';
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}]},{},[5])(5)
});
