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
},{"../lib.js":6,"../utils/validation":40,"buffer":44}],2:[function(require,module,exports){
(function (Buffer){
const bip39 = require('bip39')
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
  return bip39.generateMnemonic()
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
  kadenaMnemonicToRootKeypair,
  kadenaGenKeypair,
  kadenaGetPublic,
  kadenaSign,
  kadenaVerify,
  kadenaChangePassword
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"./key-derivation":3,"./signing":4,"bip39":8,"buffer":44}],3:[function(require,module,exports){
(function (Buffer){
const bip39 = require('bip39')

const {validateBuffer, validateDerivationIndex, validateDerivationScheme, validateMnemonic} = require("../utils/validation")
const crypto = require("./crypto-primitives")
const pbkdf2 = require('../utils/pbkdf2')
const Module = require('../lib.js')

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
  _mnemonicToSeedV1: mnemonicToSeedV1,
  _seedToKeypairV1: seedToKeypairV1,
  _seedToKeypairV2: seedToKeypairV2,
  _mnemonicToSeedV2: mnemonicToSeedV2,
}

}).call(this,require("buffer").Buffer)
},{"../lib.js":6,"../utils/pbkdf2":39,"../utils/validation":40,"./crypto-primitives":1,"bip39":8,"buffer":44}],4:[function(require,module,exports){
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
},{"../lib.js":6,"../utils/validation":40,"buffer":44}],5:[function(require,module,exports){
const kadena = require('./features/kadena-features')
const Module = require('./lib.js')

module.exports = {
  ...kadena,
}

},{"./features/kadena-features":2,"./lib.js":6}],6:[function(require,module,exports){
(function (process,Buffer,__dirname){
var Module=typeof Module!=="undefined"?Module:{};var moduleOverrides={};var key;for(key in Module){if(Module.hasOwnProperty(key)){moduleOverrides[key]=Module[key]}}var arguments_=[];var thisProgram="./this.program";var quit_=function(status,toThrow){throw toThrow};var ENVIRONMENT_IS_WEB=false;var ENVIRONMENT_IS_WORKER=false;var ENVIRONMENT_IS_NODE=false;var ENVIRONMENT_IS_SHELL=false;ENVIRONMENT_IS_WEB=typeof window==="object";ENVIRONMENT_IS_WORKER=typeof importScripts==="function";ENVIRONMENT_IS_NODE=typeof process==="object"&&typeof process.versions==="object"&&typeof process.versions.node==="string";ENVIRONMENT_IS_SHELL=!ENVIRONMENT_IS_WEB&&!ENVIRONMENT_IS_NODE&&!ENVIRONMENT_IS_WORKER;var scriptDirectory="";function locateFile(path){if(Module["locateFile"]){return Module["locateFile"](path,scriptDirectory)}return scriptDirectory+path}var read_,readAsync,readBinary,setWindowTitle;var nodeFS;var nodePath;if(ENVIRONMENT_IS_NODE){if(ENVIRONMENT_IS_WORKER){scriptDirectory=require("path").dirname(scriptDirectory)+"/"}else{scriptDirectory=__dirname+"/"}read_=function shell_read(filename,binary){var ret=tryParseAsDataURI(filename);if(ret){return binary?ret:ret.toString()}if(!nodeFS)nodeFS=require("fs");if(!nodePath)nodePath=require("path");filename=nodePath["normalize"](filename);return nodeFS["readFileSync"](filename,binary?null:"utf8")};readBinary=function readBinary(filename){var ret=read_(filename,true);if(!ret.buffer){ret=new Uint8Array(ret)}assert(ret.buffer);return ret};if(process["argv"].length>1){thisProgram=process["argv"][1].replace(/\\/g,"/")}arguments_=process["argv"].slice(2);if(typeof module!=="undefined"){module["exports"]=Module}process["on"]("unhandledRejection",abort);quit_=function(status){process["exit"](status)};Module["inspect"]=function(){return"[Emscripten Module object]"}}else if(ENVIRONMENT_IS_SHELL){if(typeof read!="undefined"){read_=function shell_read(f){var data=tryParseAsDataURI(f);if(data){return intArrayToString(data)}return read(f)}}readBinary=function readBinary(f){var data;data=tryParseAsDataURI(f);if(data){return data}if(typeof readbuffer==="function"){return new Uint8Array(readbuffer(f))}data=read(f,"binary");assert(typeof data==="object");return data};if(typeof scriptArgs!="undefined"){arguments_=scriptArgs}else if(typeof arguments!="undefined"){arguments_=arguments}if(typeof quit==="function"){quit_=function(status){quit(status)}}if(typeof print!=="undefined"){if(typeof console==="undefined")console={};console.log=print;console.warn=console.error=typeof printErr!=="undefined"?printErr:print}}else if(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER){if(ENVIRONMENT_IS_WORKER){scriptDirectory=self.location.href}else if(document.currentScript){scriptDirectory=document.currentScript.src}if(scriptDirectory.indexOf("blob:")!==0){scriptDirectory=scriptDirectory.substr(0,scriptDirectory.lastIndexOf("/")+1)}else{scriptDirectory=""}{read_=function shell_read(url){try{var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.send(null);return xhr.responseText}catch(err){var data=tryParseAsDataURI(url);if(data){return intArrayToString(data)}throw err}};if(ENVIRONMENT_IS_WORKER){readBinary=function readBinary(url){try{var xhr=new XMLHttpRequest;xhr.open("GET",url,false);xhr.responseType="arraybuffer";xhr.send(null);return new Uint8Array(xhr.response)}catch(err){var data=tryParseAsDataURI(url);if(data){return data}throw err}}}readAsync=function readAsync(url,onload,onerror){var xhr=new XMLHttpRequest;xhr.open("GET",url,true);xhr.responseType="arraybuffer";xhr.onload=function xhr_onload(){if(xhr.status==200||xhr.status==0&&xhr.response){onload(xhr.response);return}var data=tryParseAsDataURI(url);if(data){onload(data.buffer);return}onerror()};xhr.onerror=onerror;xhr.send(null)}}setWindowTitle=function(title){document.title=title}}else{}var out=Module["print"]||console.log.bind(console);var err=Module["printErr"]||console.warn.bind(console);for(key in moduleOverrides){if(moduleOverrides.hasOwnProperty(key)){Module[key]=moduleOverrides[key]}}moduleOverrides=null;if(Module["arguments"])arguments_=Module["arguments"];if(Module["thisProgram"])thisProgram=Module["thisProgram"];if(Module["quit"])quit_=Module["quit"];var wasmBinary;if(Module["wasmBinary"])wasmBinary=Module["wasmBinary"];var noExitRuntime;if(Module["noExitRuntime"])noExitRuntime=Module["noExitRuntime"];var WebAssembly={Memory:function(opts){this.buffer=new ArrayBuffer(opts["initial"]*65536);this.grow=function(amount){var ret=__growWasmMemory(amount);return ret}},Table:function(opts){var ret=new Array(opts["initial"]);ret.set=function(i,func){ret[i]=func};ret.get=function(i){return ret[i]};return ret},Module:function(binary){},Instance:function(module,info){this.exports=(
// EMSCRIPTEN_START_ASM
function a(asmLibraryArg,wasmMemory,wasmTable){function b(global,env,buffer){var memory=env.memory;var c=wasmTable;var d=new global.Int8Array(buffer);var e=new global.Int16Array(buffer);var f=new global.Int32Array(buffer);var g=new global.Uint8Array(buffer);var h=new global.Uint16Array(buffer);var i=new global.Uint32Array(buffer);var j=new global.Float32Array(buffer);var k=new global.Float64Array(buffer);var l=global.Math.imul;var m=global.Math.fround;var n=global.Math.abs;var o=global.Math.clz32;var p=global.Math.min;var q=global.Math.max;var r=global.Math.floor;var s=global.Math.ceil;var t=global.Math.sqrt;var u=env.abort;var v=global.NaN;var w=global.Infinity;var x=env.a;var y=env.b;var z=env.c;var A=5274656;var B=0;
// EMSCRIPTEN_START_FUNCS
function wa(a,b){var c=0,d=0,e=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,N=0,O=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0,ea=0,fa=0,ga=0,ha=0,ia=0,ja=0,ka=0,la=0,ma=0,na=0,oa=0,pa=0,qa=0,ra=0,sa=0,ta=0;U=A-256|0;A=U;while(1){n=Y<<3;aa=n+(U+128|0)|0;n=b+n|0;R=g[n+4|0]|g[n+5|0]<<8|(g[n+6|0]<<16|g[n+7|0]<<24);f[aa>>2]=g[n|0]|g[n+1|0]<<8|(g[n+2|0]<<16|g[n+3|0]<<24);f[aa+4>>2]=R;Y=Y+1|0;if((Y|0)!=16){continue}break}n=P(U,a,64);b=n;f[b+88>>2]=1595750129;f[b+92>>2]=-1521486534;s=f[a+88>>2];aa=f[a+92>>2];i=f[a+72>>2];R=f[a+76>>2];ba=f[a+80>>2];fa=f[a+84>>2];X=f[b+132>>2];U=b;c=f[b+36>>2];b=c+f[b+4>>2]|0;h=f[n+32>>2];Y=f[n>>2];$=h+Y|0;if($>>>0<Y>>>0){b=b+1|0}Y=$+f[n+128>>2]|0;b=b+X|0;X=Y;$=X>>>0<$>>>0?b+1|0:b;Y=M(X^f[a+64>>2]^-1377402159,$^f[a+68>>2]^1359893119,32);d=Y;b=B;m=b;b=b+1779033703|0;Y=d+ -205731576|0;if(Y>>>0<4089235720){b=b+1|0}x=Y;e=h^x;h=b;e=M(e,c^b,24);c=n;Y=f[c+136>>2];o=B;b=$+o|0;$=e+X|0;if($>>>0<X>>>0){b=b+1|0}$=Y+$|0;na=f[c+140>>2];b=na+b|0;v=$;y=v>>>0<Y>>>0?b+1|0:b;C=M(v^d,y^m,16);b=B;$=b;f[U+96>>2]=C;f[U+100>>2]=b;b=h+b|0;X=x+C|0;if(X>>>0<x>>>0){b=b+1|0}d=X;f[n+64>>2]=d;f[U+68>>2]=b;z=b;X=M(e^d,o^b,63);b=B;m=b;f[n+32>>2]=X;f[n+36>>2]=b;b=f[n+148>>2];ka=b;x=b;j=i;i=f[n+44>>2];b=i+f[n+12>>2]|0;e=f[n+40>>2];c=f[n+8>>2];o=e+c|0;if(o>>>0<c>>>0){b=b+1|0}h=f[n+144>>2];c=h+o|0;b=b+x|0;x=c;k=R;R=c>>>0<o>>>0?b+1|0:b;c=M(j^c^725511199,k^R^-1694144372,32);o=c;b=B;j=b;b=b+ -1150833019|0;c=c+ -2067093701|0;if(c>>>0<2227873595){b=b+1|0}k=e^c;e=b;i=M(k,i^b,24);Z=B;b=R+Z|0;R=i+x|0;if(R>>>0<x>>>0){b=b+1|0}x=f[n+152>>2];R=x+R|0;b=f[n+156>>2]+b|0;I=R;N=I>>>0<x>>>0?b+1|0:b;T=M(I^o,N^j,16);b=B;o=b;f[U+104>>2]=T;f[U+108>>2]=b;b=e+b|0;R=c+T|0;if(R>>>0<c>>>0){b=b+1|0}j=R;f[n+72>>2]=j;f[U+76>>2]=b;H=b;x=M(i^j,Z^b,63);c=B;da=f[n+164>>2];R=da;k=ba;ba=f[n+52>>2];b=ba+f[n+20>>2]|0;i=f[n+48>>2];U=f[n+16>>2];e=i+U|0;if(e>>>0<U>>>0){b=b+1|0}K=f[n+160>>2];U=K+e|0;b=b+R|0;q=U;R=q>>>0<e>>>0?b+1|0:b;l=M(k^q^-79577749,R^fa^528734635,32);b=B;fa=b;b=b+1013904242|0;U=l+ -23791573|0;if(U>>>0<4271175723){b=b+1|0}e=U;k=i^e;i=b;U=M(k,ba^b,24);b=B;ba=U;Z=b;k=fa;oa=f[n+172>>2];p=oa;b=b+R|0;R=q+U|0;if(R>>>0<U>>>0){b=b+1|0}fa=f[n+168>>2];U=fa+R|0;b=b+p|0;D=U;J=D>>>0<R>>>0?b+1|0:b;O=M(D^l,J^k,16);G=B;b=i+G|0;U=e+O|0;if(U>>>0<e>>>0){b=b+1|0}q=U;t=b;e=M(q^ba,b^Z,63);i=B;pa=f[n+180>>2];ba=pa;p=s;l=f[n+60>>2];b=l+f[n+28>>2]|0;s=f[n+56>>2];R=f[n+24>>2];Z=s+R|0;if(Z>>>0<R>>>0){b=b+1|0}U=f[n+176>>2];R=U+Z|0;b=b+ba|0;b=R>>>0<Z>>>0?b+1|0:b;Z=R;k=b;u=M(p^R^327033209,b^aa^1541459225,32);b=B;aa=b;b=b+ -1521486534|0;R=u+1595750129|0;if(R>>>0<1595750129){b=b+1|0}ba=R;R=M(s^R,b^l,24);s=R;p=ba;r=b;w=aa;ba=f[n+188>>2];L=ba;l=B;b=k+l|0;R=s+Z|0;if(R>>>0<Z>>>0){b=b+1|0}aa=f[n+184>>2];Z=R;R=aa+R|0;b=b+L|0;V=R;k=w;w=R>>>0<Z>>>0?b+1|0:b;Z=M(R^u,k^w,16);E=s;s=B;b=s+r|0;R=Z;k=R+p|0;if(k>>>0<R>>>0){b=b+1|0}u=k;R=b;k=M(E^k,b^l,63);l=B;ea=f[n+196>>2];p=ea;E=Z;b=c+y|0;y=v+x|0;if(y>>>0<x>>>0){b=b+1|0}ga=f[n+192>>2];Z=ga+y|0;b=b+p|0;p=Z;r=s;s=p>>>0<y>>>0?b+1|0:b;Z=M(E^p,r^s,32);b=B;y=n;v=b;E=x;b=t+b|0;r=Z;x=q+r|0;if(x>>>0<q>>>0){b=b+1|0}t=c;c=b;E=M(E^x,t^b,24);Z=n;q=f[n+200>>2];L=r;r=B;b=s+r|0;t=p+E|0;if(t>>>0<p>>>0){b=b+1|0}s=q+t|0;ca=f[Z+204>>2];b=ca+b|0;ha=s;t=v;v=s>>>0<q>>>0?b+1|0:b;W=M(L^s,t^v,16);b=B;t=b;f[y+120>>2]=W;f[y+124>>2]=b;b=c+b|0;c=x+W|0;if(c>>>0<x>>>0){b=b+1|0}s=c;f[n+80>>2]=c;f[Z+84>>2]=b;c=c^E;E=b;p=M(c,r^b,63);r=B;Z=i;ma=f[n+212>>2];y=ma;L=C;b=i+N|0;c=e;e=I+c|0;if(e>>>0<I>>>0){b=b+1|0}x=f[n+208>>2];i=e;e=x+e|0;b=b+y|0;C=e;e=e>>>0<i>>>0?b+1|0:b;$=M(L^C,e^$,32);I=c;c=B;b=c+R|0;R=$;i=R+u|0;if(i>>>0<R>>>0){b=b+1|0}R=M(I^i,b^Z,24);I=R;N=b;ra=f[n+220>>2];L=ra;F=$;Z=B;b=Z+e|0;$=C+I|0;if($>>>0<I>>>0){b=b+1|0}y=f[n+216>>2];R=y+$|0;b=b+L|0;u=R;C=u>>>0<$>>>0?b+1|0:b;c=M(F^u,C^c,16);b=N;N=B;b=b+N|0;$=c+i|0;if($>>>0<c>>>0){b=b+1|0}L=$;i=b;e=M(L^I,b^Z,63);I=B;R=k;S=l;Z=f[n+228>>2];Q=Z;b=l+J|0;k=k+D|0;if(k>>>0<D>>>0){b=b+1|0}$=f[n+224>>2];l=k;k=$+k|0;b=b+Q|0;Q=k;k=k>>>0<l>>>0?b+1|0:b;T=M(Q^T,k^o,32);l=R;o=B;b=z+o|0;R=d+T|0;if(R>>>0<d>>>0){b=b+1|0}d=R;R=M(l^d,b^S,24);z=R;D=b;J=o;ia=f[n+236>>2];F=ia;l=B;b=l+k|0;k=z+Q|0;if(k>>>0<z>>>0){b=b+1|0}o=f[n+232>>2];R=o+k|0;b=b+F|0;S=R;F=J;J=R>>>0<k>>>0?b+1|0:b;k=M(R^T,F^J,16);b=D;D=B;b=b+D|0;d=d+k|0;if(d>>>0<k>>>0){b=b+1|0}T=d;d=b;l=M(T^z,b^l,63);z=B;F=m;qa=f[n+244>>2];la=qa;b=m+w|0;Q=X;m=Q+V|0;if(m>>>0<Q>>>0){b=b+1|0}R=f[n+240>>2];X=R+m|0;b=b+la|0;b=X>>>0<m>>>0?b+1|0:b;m=X;w=G;G=b;O=M(m^O,w^b,32);w=d;d=c;X=B;b=H+X|0;c=j+O|0;if(c>>>0<j>>>0){b=b+1|0}j=b;Q=M(c^Q,b^F,24);F=Q;b=B;V=b;H=X;la=f[n+252>>2];ja=la;b=b+G|0;G=m+F|0;if(G>>>0<m>>>0){b=b+1|0}X=f[n+248>>2];m=X+G|0;b=b+ja|0;b=m>>>0<G>>>0?b+1|0:b;G=m;Q=H;H=b;ja=M(m^O,Q^b,16);O=F;Q=B;b=j+Q|0;m=c+ja|0;if(m>>>0<c>>>0){b=b+1|0}F=m;c=b;O=M(O^m,b^V,63);b=B;V=b;m=b;_=d;b=v+qa|0;d=R;j=d+ha|0;if(j>>>0<d>>>0){b=b+1|0}d=j+O|0;b=b+m|0;m=d;v=N;N=d>>>0<j>>>0?b+1|0:b;d=M(_^d,v^N,32);j=n;b=w;w=B;b=b+w|0;T=d+T|0;if(T>>>0<d>>>0){b=b+1|0}v=T;T=b;V=M(O^v,V^b,24);_=V;b=B;sa=b;O=b;ha=d;b=N+ma|0;d=m+x|0;if(d>>>0<m>>>0){b=b+1|0}m=d;d=d+V|0;b=b+O|0;ta=d;N=w;w=d>>>0<m>>>0?b+1|0:b;ha=M(ha^d,N^w,16);O=B;b=T+O|0;d=v+ha|0;if(d>>>0<v>>>0){b=b+1|0}v=d;T=b;m=M(d^_,b^sa,63);b=B;N=b;f[j+32>>2]=m;f[j+36>>2]=b;V=c;_=k;j=r;b=da+j|0;d=p;c=K+d|0;if(c>>>0<K>>>0){b=b+1|0}c=c+u|0;b=b+C|0;k=c;p=c>>>0<u>>>0?b+1|0:b;c=M(_^c,p^D,32);C=d;d=B;b=d+V|0;K=c+F|0;if(K>>>0<c>>>0){b=b+1|0}r=K;u=j;j=b;D=M(C^r,u^b,24);u=D;b=B;F=b;K=b;C=c;b=p+ea|0;c=k+ga|0;if(c>>>0<k>>>0){b=b+1|0}k=c;c=c+D|0;b=b+K|0;K=c;D=c>>>0<k>>>0?b+1|0:b;V=M(C^c,D^d,16);k=u;u=B;b=j+u|0;c=r+V|0;if(c>>>0<r>>>0){b=b+1|0}C=c;d=F;F=b;j=M(k^c,d^b,63);c=B;k=I;b=k+ca|0;d=e;q=d+q|0;if(q>>>0<d>>>0){b=b+1|0}e=q+S|0;b=b+J|0;r=e;e=e>>>0<S>>>0?b+1|0:b;p=M(r^ja,e^Q,32);J=d;d=B;b=E+d|0;q=p+s|0;if(q>>>0<s>>>0){b=b+1|0}s=k;k=b;I=M(J^q,s^b,24);J=I;b=B;E=b;s=b;S=p;b=e+la|0;e=X;p=e+r|0;if(p>>>0<e>>>0){b=b+1|0}e=p+I|0;b=b+s|0;r=e;I=e>>>0<p>>>0?b+1|0:b;da=M(S^e,I^d,16);p=J;J=B;b=k+J|0;e=q+da|0;if(e>>>0<q>>>0){b=b+1|0}s=e;d=b;k=M(p^e,b^E,63);q=B;p=l;E=z;b=z+ia|0;e=o;l=e+l|0;if(l>>>0<e>>>0){b=b+1|0}e=l+G|0;b=b+H|0;H=e;l=e>>>0<G>>>0?b+1|0:b;e=M(e^W,l^t,32);z=p;b=i;i=B;b=b+i|0;p=e+L|0;if(p>>>0<e>>>0){b=b+1|0}t=p;p=b;G=M(z^t,b^E,24);E=G;b=B;L=b;z=b;S=e;b=l+pa|0;e=U;l=e+H|0;if(l>>>0<e>>>0){b=b+1|0}e=l+G|0;b=b+z|0;z=e;H=e>>>0<l>>>0?b+1|0:b;S=M(S^e,H^i,16);l=E;G=B;b=p+G|0;e=t+S|0;if(e>>>0<t>>>0){b=b+1|0}E=e;i=b;l=M(l^e,b^L,63);e=B;t=n;p=j;L=c;Q=j;b=w+na|0;j=Y;w=j+ta|0;if(w>>>0<j>>>0){b=b+1|0}j=Q+w|0;b=b+c|0;b=j>>>0<w>>>0?b+1|0:b;w=j;j=b;c=M(w^S,b^G,32);S=B;b=S+d|0;s=c+s|0;if(s>>>0<c>>>0){b=b+1|0}d=s;s=d^p;p=b;L=M(s,b^L,24);b=B;G=b;s=b;b=j+Z|0;j=$;w=j+w|0;if(w>>>0<j>>>0){b=b+1|0}j=w+L|0;b=b+s|0;b=j>>>0<w>>>0?b+1|0:b;w=b;ja=M(c^j,S^b,16);b=B;s=b;f[t+120>>2]=ja;f[t+124>>2]=b;t=L;b=p+b|0;c=d+ja|0;if(c>>>0<d>>>0){b=b+1|0}L=c;d=b;t=M(t^c,b^G,63);p=B;G=k;S=q;ca=f[n+132>>2];W=ca;b=q+D|0;c=k+K|0;if(c>>>0<K>>>0){b=b+1|0}Q=f[n+128>>2];k=c;c=Q+c|0;b=b+W|0;b=c>>>0<k>>>0?b+1|0:b;k=c;q=b;c=M(c^ha,b^O,32);b=i;i=B;b=b+i|0;D=c+E|0;if(D>>>0<c>>>0){b=b+1|0}K=D;E=M(D^G,b^S,24);S=E;D=B;O=D;G=b;W=c;b=q+ka|0;c=h+k|0;if(c>>>0<k>>>0){b=b+1|0}k=c;c=c+E|0;b=b+D|0;D=c;E=c>>>0<k>>>0?b+1|0:b;c=M(W^c,E^i,16);b=G;G=B;b=b+G|0;k=c+K|0;if(k>>>0<c>>>0){b=b+1|0}K=k;i=k^S;S=b;i=M(i,b^O,63);k=B;q=l;O=e;W=V;V=l;b=I+ra|0;l=r+y|0;if(l>>>0<r>>>0){b=b+1|0}r=l;l=V+l|0;b=b+e|0;V=l;e=l>>>0<r>>>0?b+1|0:b;u=M(W^l,e^u,32);r=q;l=B;b=T+l|0;q=v+u|0;if(q>>>0<v>>>0){b=b+1|0}v=M(r^q,b^O,24);T=v;r=B;O=r;I=b;W=u;b=e+ba|0;e=aa;u=e+V|0;if(u>>>0<e>>>0){b=b+1|0}e=u+v|0;b=b+r|0;b=e>>>0<u>>>0?b+1|0:b;u=e;v=b;l=M(W^e,b^l,16);r=T;T=B;b=T+I|0;e=l;q=e+q|0;if(q>>>0<e>>>0){b=b+1|0}I=q;e=b;q=M(r^q,b^O,63);r=B;O=m;V=N;b=N+oa|0;m=fa;N=O+m|0;if(N>>>0<m>>>0){b=b+1|0}m=z+N|0;b=b+H|0;b=m>>>0<z>>>0?b+1|0:b;z=b;W=M(m^da,b^J,32);N=I;H=e;I=c;c=B;b=F+c|0;e=C+W|0;if(e>>>0<C>>>0){b=b+1|0}F=M(e^O,b^V,24);O=F;V=B;da=V;J=b;C=c;ha=f[n+156>>2];_=ha;b=z+V|0;z=m+F|0;if(z>>>0<m>>>0){b=b+1|0}c=f[n+152>>2];m=c+z|0;b=b+_|0;F=m;V=C;C=m>>>0<z>>>0?b+1|0:b;m=M(m^W,V^C,16);V=O;b=J;J=B;b=b+J|0;z=e;e=m;z=z+e|0;if(z>>>0<e>>>0){b=b+1|0}O=z;e=b;V=M(V^z,b^da,63);b=B;W=b;z=b;_=I;b=w+ra|0;I=j+y|0;if(I>>>0<j>>>0){b=b+1|0}j=I+V|0;b=b+z|0;z=j;w=G;G=j>>>0<I>>>0?b+1|0:b;j=M(_^j,w^G,32);I=n;b=H;H=B;b=b+H|0;w=j+N|0;if(w>>>0<j>>>0){b=b+1|0}N=w;w=b;W=M(V^N,W^b,24);da=W;b=B;_=b;V=b;sa=j;b=G+ea|0;j=z+ga|0;if(j>>>0<z>>>0){b=b+1|0}z=j;j=j+W|0;b=b+V|0;ga=j;G=H;H=j>>>0<z>>>0?b+1|0:b;ea=M(sa^j,G^H,16);G=B;b=w+G|0;j=N+ea|0;if(j>>>0<N>>>0){b=b+1|0}N=j;w=b;j=M(j^da,b^_,63);b=B;z=b;f[I+32>>2]=j;f[I+36>>2]=b;I=t;V=p;W=e;_=l;b=p+Z|0;e=$;l=e+t|0;if(l>>>0<e>>>0){b=b+1|0}e=l+D|0;b=b+E|0;E=e;l=e>>>0<D>>>0?b+1|0:b;e=M(_^e,l^T,32);D=I;t=B;b=t+W|0;I=e+O|0;if(I>>>0<e>>>0){b=b+1|0}p=I;I=b;T=M(D^p,b^V,24);V=T;b=B;W=b;D=b;O=e;b=l+ca|0;e=E+Q|0;if(e>>>0<Q>>>0){b=b+1|0}l=e;e=e+T|0;b=b+D|0;da=e;D=e>>>0<l>>>0?b+1|0:b;O=M(O^e,D^t,16);E=B;b=I+E|0;e=p+O|0;if(e>>>0<p>>>0){b=b+1|0}I=e;T=b;l=M(e^V,b^W,63);e=B;t=i;p=k;Q=m;b=k+oa|0;i=fa;m=t+i|0;if(m>>>0<i>>>0){b=b+1|0}i=m+u|0;b=b+v|0;b=i>>>0<u>>>0?b+1|0:b;u=i;m=b;i=M(Q^i,b^J,32);J=t;b=d;d=B;b=b+d|0;t=i+L|0;if(t>>>0<i>>>0){b=b+1|0}k=t;t=b;J=M(J^k,b^p,24);v=J;b=B;L=b;p=b;Q=i;b=m+ka|0;m=h+u|0;if(m>>>0<h>>>0){b=b+1|0}i=m+J|0;b=b+p|0;W=i;p=i>>>0<m>>>0?b+1|0:b;V=M(Q^i,p^d,16);J=B;b=t+J|0;i=k+V|0;if(i>>>0<k>>>0){b=b+1|0}u=i;m=b;k=M(i^v,b^L,63);i=B;t=r;b=la+t|0;d=q;q=X;r=d+q|0;if(r>>>0<q>>>0){b=b+1|0}q=r+F|0;b=b+C|0;b=q>>>0<F>>>0?b+1|0:b;r=s;s=b;v=M(q^ja,r^b,32);L=d;d=B;b=S+d|0;r=v+K|0;if(r>>>0<K>>>0){b=b+1|0}C=t;t=b;C=M(L^r,C^b,24);L=C;b=B;S=b;K=b;b=s+ia|0;s=o+q|0;if(s>>>0<q>>>0){b=b+1|0}q=s+C|0;b=b+K|0;K=q;C=q^v;v=q>>>0<s>>>0?b+1|0:b;Q=M(C,v^d,16);s=B;b=t+s|0;d=r+Q|0;if(d>>>0<r>>>0){b=b+1|0}r=d;C=b;q=M(d^L,b^S,63);d=B;t=n;L=l;S=e;_=Q;F=l;b=H+ma|0;l=x;H=l+ga|0;if(H>>>0<l>>>0){b=b+1|0}l=F+H|0;b=b+e|0;Q=l;l=l>>>0<H>>>0?b+1|0:b;e=M(_^Q,l^s,32);F=B;b=F+m|0;s=e+u|0;if(s>>>0<e>>>0){b=b+1|0}m=s;s=b;L=M(m^L,b^S,24);b=B;H=b;u=b;b=l+qa|0;l=R;S=l+Q|0;if(S>>>0<l>>>0){b=b+1|0}l=S+L|0;b=b+u|0;ca=l;u=l>>>0<S>>>0?b+1|0:b;ja=M(e^l,F^u,16);b=B;l=b;f[t+120>>2]=ja;f[t+124>>2]=b;t=L;b=b+s|0;e=m+ja|0;if(e>>>0<m>>>0){b=b+1|0}L=e;m=b;t=M(t^e,b^H,63);s=B;H=i;b=D+ha|0;D=c+da|0;if(D>>>0<c>>>0){b=b+1|0}e=k;k=e+D|0;b=b+i|0;S=k;i=k>>>0<D>>>0?b+1|0:b;D=M(k^ea,i^G,32);G=e;e=B;b=C+e|0;k=r+D|0;if(k>>>0<r>>>0){b=b+1|0}G=M(G^k,b^H,24);C=G;r=B;Q=r;H=b;F=D;b=i+pa|0;i=U;D=i+S|0;if(D>>>0<i>>>0){b=b+1|0}i=D+G|0;b=b+r|0;b=i>>>0<D>>>0?b+1|0:b;D=i;G=b;e=M(F^i,b^e,16);i=C;b=H;H=B;b=b+H|0;k=e+k|0;if(k>>>0<e>>>0){b=b+1|0}C=k;S=b;i=M(i^k,b^Q,63);k=B;r=q;Q=d;F=q;b=p+ba|0;q=aa;p=q+W|0;if(p>>>0<q>>>0){b=b+1|0}q=F+p|0;b=b+d|0;F=q;d=q>>>0<p>>>0?b+1|0:b;E=M(q^O,d^E,32);q=B;b=w+q|0;p=E+N|0;if(p>>>0<N>>>0){b=b+1|0}w=M(p^r,b^Q,24);Q=w;r=B;O=r;N=b;W=w;b=d+na|0;d=Y;w=d+F|0;if(w>>>0<d>>>0){b=b+1|0}d=W+w|0;b=b+r|0;b=d>>>0<w>>>0?b+1|0:b;w=d;d=d^E;E=b;q=M(d,b^q,16);r=Q;Q=B;b=Q+N|0;d=q;p=d+p|0;if(p>>>0<d>>>0){b=b+1|0}N=p;d=b;p=M(r^p,b^O,63);r=B;F=j;O=z;ea=f[n+204>>2];W=ea;b=v+z|0;j=j+K|0;if(j>>>0<K>>>0){b=b+1|0}ga=f[n+200>>2];z=j;j=ga+j|0;b=b+W|0;b=j>>>0<z>>>0?b+1|0:b;z=b;V=M(j^V,b^J,32);K=N;J=d;d=e;e=B;b=T+e|0;N=I+V|0;if(N>>>0<I>>>0){b=b+1|0}I=N;T=M(I^F,b^O,24);O=T;F=B;W=F;v=b;da=f[n+164>>2];_=da;b=z+F|0;z=j+T|0;if(z>>>0<j>>>0){b=b+1|0}I=f[n+160>>2];j=I+z|0;b=b+_|0;T=j;F=j>>>0<z>>>0?b+1|0:b;z=M(j^V,F^e,16);V=O;b=v;v=B;b=b+v|0;e=z;j=e+N|0;if(j>>>0<e>>>0){b=b+1|0}O=j;e=b;V=M(V^j,b^W,63);b=B;W=b;j=b;_=d;b=u+ba|0;d=aa;N=d+ca|0;if(N>>>0<d>>>0){b=b+1|0}d=N+V|0;b=b+j|0;j=d;u=H;H=d>>>0<N>>>0?b+1|0:b;d=M(_^d,u^H,32);N=n;b=J;J=B;b=b+J|0;u=d+K|0;if(u>>>0<d>>>0){b=b+1|0}K=u;W=M(V^u,b^W,24);ca=W;u=B;_=u;V=b;ta=d;b=H+ea|0;d=j+ga|0;if(d>>>0<j>>>0){b=b+1|0}j=d;d=d+W|0;b=b+u|0;sa=d;H=J;J=d>>>0<j>>>0?b+1|0:b;H=M(ta^d,H^J,16);u=B;b=u+V|0;d=H;j=d+K|0;if(j>>>0<d>>>0){b=b+1|0}V=j;d=b;j=M(j^ca,b^_,63);b=B;K=b;f[N+32>>2]=j;f[N+36>>2]=b;N=t;W=s;_=q;b=s+ha|0;q=c+t|0;if(q>>>0<c>>>0){b=b+1|0}c=q+D|0;b=b+G|0;b=c>>>0<D>>>0?b+1|0:b;D=c;q=b;c=M(_^c,b^Q,32);b=e;e=B;b=b+e|0;s=c+O|0;if(s>>>0<c>>>0){b=b+1|0}t=s;G=M(s^N,b^W,24);Q=G;s=B;O=s;N=b;W=c;b=q+na|0;c=Y;q=c+D|0;if(q>>>0<c>>>0){b=b+1|0}c=q+G|0;b=b+s|0;ha=c;D=c>>>0<q>>>0?b+1|0:b;q=M(W^c,D^e,16);b=N;N=B;b=b+N|0;c=q;e=c+t|0;if(e>>>0<c>>>0){b=b+1|0}G=e;c=e^Q;Q=b;t=M(c,b^O,63);c=B;s=i;O=k;b=k+ia|0;e=i+o|0;if(e>>>0<i>>>0){b=b+1|0}e=e+w|0;b=b+E|0;i=e;o=e>>>0<w>>>0?b+1|0:b;e=M(e^z,o^v,32);z=s;b=m;m=B;b=b+m|0;s=e+L|0;if(s>>>0<e>>>0){b=b+1|0}k=s;s=b;w=M(z^k,b^O,24);v=w;b=B;E=b;z=b;L=e;b=o+Z|0;e=i+$|0;if(e>>>0<i>>>0){b=b+1|0}i=e;e=e+w|0;b=b+z|0;O=e;z=e>>>0<i>>>0?b+1|0:b;ia=M(L^e,z^m,16);w=B;b=s+w|0;e=k+ia|0;if(e>>>0<k>>>0){b=b+1|0}s=e;m=b;o=M(e^v,b^E,63);e=B;k=r;b=ra+k|0;i=p;p=y;r=i+p|0;if(r>>>0<p>>>0){b=b+1|0}p=r+T|0;b=b+F|0;E=p;r=l;l=p>>>0<T>>>0?b+1|0:b;v=M(p^ja,r^l,32);L=i;i=B;b=S+i|0;p=v+C|0;if(p>>>0<C>>>0){b=b+1|0}r=k;k=b;C=M(L^p,r^b,24);L=C;b=B;S=b;r=b;F=v;b=l+qa|0;l=R;v=l+E|0;if(v>>>0<l>>>0){b=b+1|0}l=v+C|0;b=b+r|0;b=l>>>0<v>>>0?b+1|0:b;v=l;E=b;T=M(F^l,b^i,16);r=B;b=k+r|0;i=p+T|0;if(i>>>0<p>>>0){b=b+1|0}p=i;C=b;k=M(i^L,b^S,63);i=B;l=n;L=t;S=c;F=T;b=J+ka|0;J=h+sa|0;if(J>>>0<h>>>0){b=b+1|0}t=J+t|0;b=b+c|0;T=t;t=t>>>0<J>>>0?b+1|0:b;c=M(F^T,t^r,32);F=B;b=F+m|0;s=c+s|0;if(s>>>0<c>>>0){b=b+1|0}m=s;s=b;S=M(m^L,b^S,24);b=B;r=b;J=b;b=t+pa|0;t=U;L=t+T|0;if(L>>>0<t>>>0){b=b+1|0}t=L+S|0;b=b+J|0;J=t;L=t>>>0<L>>>0?b+1|0:b;W=M(c^t,F^L,16);b=B;t=b;f[l+120>>2]=W;f[l+124>>2]=b;l=S;b=s+b|0;c=m+W|0;if(c>>>0<m>>>0){b=b+1|0}S=c;m=b;l=M(l^c,b^r,63);s=B;r=e;T=H;c=o;F=c;b=D+oa|0;o=fa;H=o+ha|0;if(H>>>0<o>>>0){b=b+1|0}o=F+H|0;b=b+e|0;D=o;e=o>>>0<H>>>0?b+1|0:b;H=M(T^o,e^u,32);u=c;c=B;b=C+c|0;o=p+H|0;if(o>>>0<p>>>0){b=b+1|0}u=M(u^o,b^r,24);C=u;p=B;T=p;r=b;F=H;b=e+ma|0;e=x;H=e+D|0;if(H>>>0<e>>>0){b=b+1|0}e=H+u|0;b=b+p|0;D=e;u=e>>>0<H>>>0?b+1|0:b;c=M(F^e,u^c,16);p=C;H=B;b=H+r|0;o=c+o|0;if(o>>>0<c>>>0){b=b+1|0}C=o;e=T;T=b;p=M(p^o,e^b,63);r=B;o=k;F=i;_=k;b=z+da|0;e=I;k=e+O|0;if(k>>>0<e>>>0){b=b+1|0}e=_+k|0;b=b+i|0;b=e>>>0<k>>>0?b+1|0:b;k=e;e=e^q;q=b;e=M(e,b^N,32);i=o;b=d;d=B;b=b+d|0;o=e+V|0;if(o>>>0<e>>>0){b=b+1|0}i=M(i^o,b^F,24);N=i;F=b;ja=f[n+132>>2];V=ja;_=e;z=B;b=q+z|0;i=i+k|0;if(i>>>0<k>>>0){b=b+1|0}e=f[n+128>>2];k=i;i=e+i|0;b=b+V|0;O=i;V=i>>>0<k>>>0?b+1|0:b;d=M(_^i,V^d,16);b=F;F=B;b=b+F|0;o=d+o|0;if(o>>>0<d>>>0){b=b+1|0}i=b;q=M(o^N,b^z,63);z=B;k=j;N=K;b=K+la|0;j=X;K=k+j|0;if(K>>>0<j>>>0){b=b+1|0}j=v+K|0;b=b+E|0;b=j>>>0<v>>>0?b+1|0:b;K=b;v=M(j^ia,b^w,32);w=i;i=c;E=k;c=B;b=Q+c|0;k=v+G|0;if(k>>>0<G>>>0){b=b+1|0}G=b;E=M(E^k,b^N,24);Q=E;b=B;ca=b;ha=f[n+196>>2];ia=ha;b=b+K|0;K=j+E|0;if(K>>>0<j>>>0){b=b+1|0}N=f[n+192>>2];j=N+K|0;b=b+ia|0;b=j>>>0<K>>>0?b+1|0:b;K=j;j=j^v;v=b;ia=M(j,b^c,16);E=B;b=G+E|0;c=k+ia|0;if(c>>>0<k>>>0){b=b+1|0}G=c;c=b;Q=M(G^Q,b^ca,63);b=B;ca=b;j=b;_=i;b=L+ea|0;i=J+ga|0;if(i>>>0<J>>>0){b=b+1|0}k=i;i=i+Q|0;b=b+j|0;L=i;k=i>>>0<k>>>0?b+1|0:b;i=M(_^i,k^H,32);j=n;H=B;b=H+w|0;J=i+o|0;if(J>>>0<i>>>0){b=b+1|0}o=J;J=b;Q=M(Q^o,ca^b,24);ga=Q;b=B;ca=b;w=b;ea=i;b=k+ja|0;k=e+L|0;if(k>>>0<e>>>0){b=b+1|0}i=k+Q|0;b=b+w|0;_=i;w=i>>>0<k>>>0?b+1|0:b;ea=M(ea^i,w^H,16);L=B;b=J+L|0;i=o+ea|0;if(i>>>0<o>>>0){b=b+1|0}J=i;o=b;k=M(i^ga,b^ca,63);b=B;H=b;f[j+32>>2]=k;f[j+36>>2]=b;Q=c;ca=d;j=s;b=oa+j|0;i=l;c=fa;d=i+c|0;if(d>>>0<c>>>0){b=b+1|0}c=d+D|0;b=b+u|0;b=c>>>0<D>>>0?b+1|0:b;D=c;d=b;c=M(ca^c,b^F,32);u=i;i=B;b=i+Q|0;s=c+G|0;if(s>>>0<c>>>0){b=b+1|0}l=s;s=j;j=b;G=M(u^l,s^b,24);u=G;b=B;F=b;s=b;Q=c;b=d+ba|0;c=aa;d=c+D|0;if(d>>>0<c>>>0){b=b+1|0}c=d+G|0;b=b+s|0;D=c;G=c>>>0<d>>>0?b+1|0:b;ga=M(Q^c,G^i,16);d=u;u=B;b=j+u|0;c=l+ga|0;if(c>>>0<l>>>0){b=b+1|0}Q=c;j=b;l=M(d^c,b^F,63);c=B;s=r;b=ka+s|0;d=p;p=h+d|0;if(p>>>0<h>>>0){b=b+1|0}i=p+O|0;b=b+V|0;p=i;r=i>>>0<O>>>0?b+1|0:b;i=M(i^ia,r^E,32);F=d;d=B;b=d+m|0;E=i+S|0;if(E>>>0<i>>>0){b=b+1|0}m=E;E=s;s=b;S=M(F^m,E^b,24);F=S;b=B;O=b;E=b;V=i;b=r+da|0;i=p+I|0;if(i>>>0<p>>>0){b=b+1|0}p=i;i=i+S|0;b=b+E|0;da=i;r=i>>>0<p>>>0?b+1|0:b;V=M(V^i,r^d,16);I=B;b=s+I|0;i=m+V|0;if(i>>>0<m>>>0){b=b+1|0}p=i;m=b;s=M(i^F,b^O,63);i=B;E=z;b=z+ma|0;d=q;q=x;z=d+q|0;if(z>>>0<q>>>0){b=b+1|0}q=z+K|0;b=b+v|0;S=q;q=q>>>0<K>>>0?b+1|0:b;v=M(S^W,q^t,32);F=d;d=B;b=T+d|0;t=v+C|0;if(t>>>0<C>>>0){b=b+1|0}z=b;E=M(F^t,b^E,24);C=E;b=B;T=b;K=b;F=v;b=q+la|0;q=X;v=q+S|0;if(v>>>0<q>>>0){b=b+1|0}q=v+E|0;b=b+K|0;K=q;v=q>>>0<v>>>0?b+1|0:b;F=M(F^q,v^d,16);E=B;b=z+E|0;d=t+F|0;if(d>>>0<t>>>0){b=b+1|0}z=d;d=d^C;C=b;q=M(d,b^T,63);d=B;t=n;S=l;T=c;O=F;F=l;b=w+qa|0;l=R;w=l+_|0;if(w>>>0<l>>>0){b=b+1|0}l=F+w|0;b=b+c|0;F=l;l=l>>>0<w>>>0?b+1|0:b;c=M(O^F,l^E,32);O=B;b=O+m|0;p=c+p|0;if(p>>>0<c>>>0){b=b+1|0}w=b;T=M(p^S,b^T,24);b=B;E=b;m=b;b=l+na|0;l=Y;S=l+F|0;if(S>>>0<l>>>0){b=b+1|0}l=S+T|0;b=b+m|0;ca=l;S=l>>>0<S>>>0?b+1|0:b;ia=M(c^l,O^S,16);b=B;m=b;f[t+120>>2]=ia;f[t+124>>2]=b;b=b+w|0;c=p+ia|0;if(c>>>0<p>>>0){b=b+1|0}w=c;t=E;E=b;l=M(c^T,t^b,63);t=B;p=i;c=s;F=c;b=G+ra|0;s=y+D|0;if(s>>>0<D>>>0){b=b+1|0}D=s;s=F+s|0;b=b+i|0;G=s;i=s>>>0<D>>>0?b+1|0:b;D=M(s^ea,i^L,32);L=c;c=B;b=C+c|0;s=z+D|0;if(s>>>0<z>>>0){b=b+1|0}z=p;p=b;C=M(L^s,z^b,24);L=C;b=B;T=b;z=b;F=D;b=i+Z|0;i=$;D=i+G|0;if(D>>>0<i>>>0){b=b+1|0}i=D+C|0;b=b+z|0;z=i;D=i>>>0<D>>>0?b+1|0:b;i=M(F^i,D^c,16);G=B;b=p+G|0;c=i+s|0;if(c>>>0<s>>>0){b=b+1|0}C=c;c=c^L;L=b;s=M(c,b^T,63);p=B;T=q;F=d;O=q;b=r+pa|0;c=U;q=c+da|0;if(q>>>0<c>>>0){b=b+1|0}c=O+q|0;b=b+d|0;d=c;q=c>>>0<q>>>0?b+1|0:b;c=M(c^ga,q^u,32);b=o;o=B;b=b+o|0;J=c+J|0;if(J>>>0<c>>>0){b=b+1|0}r=J;J=b;T=M(r^T,b^F,24);F=T;b=B;O=b;u=b;W=c;b=q+ha|0;c=d+N|0;if(c>>>0<d>>>0){b=b+1|0}d=c;c=c+T|0;b=b+u|0;u=c;T=c>>>0<d>>>0?b+1|0:b;ea=M(W^c,T^o,16);d=F;F=B;b=J+F|0;c=r+ea|0;if(c>>>0<r>>>0){b=b+1|0}r=c;c=b;o=M(d^r,b^O,63);q=B;J=k;O=H;ga=f[n+156>>2];W=ga;_=V;b=v+H|0;d=k+K|0;if(d>>>0<K>>>0){b=b+1|0}V=f[n+152>>2];k=d;d=V+d|0;b=b+W|0;b=d>>>0<k>>>0?b+1|0:b;k=d;H=I;I=b;d=M(_^d,H^b,32);H=c;c=B;b=c+j|0;K=d+Q|0;if(K>>>0<d>>>0){b=b+1|0}j=K;v=M(j^J,b^O,24);Q=v;O=B;W=O;K=b;da=f[n+236>>2];_=da;sa=d;b=I+O|0;d=k+v|0;if(d>>>0<k>>>0){b=b+1|0}J=f[n+232>>2];k=d;d=J+d|0;b=b+_|0;I=d;v=d>>>0<k>>>0?b+1|0:b;c=M(sa^d,v^c,16);d=Q;b=K;K=B;b=b+K|0;j=c+j|0;if(j>>>0<c>>>0){b=b+1|0}Q=j;O=b;W=M(d^j,b^W,63);b=B;_=b;d=b;k=i;b=S+ka|0;i=h+ca|0;if(i>>>0<h>>>0){b=b+1|0}h=i+W|0;b=b+d|0;S=h;d=h>>>0<i>>>0?b+1|0:b;h=M(k^h,d^G,32);i=n;j=B;b=j+H|0;r=h+r|0;if(r>>>0<h>>>0){b=b+1|0}k=r;G=M(W^k,b^_,24);W=G;r=B;ka=r;H=b;_=h;b=d+Z|0;h=$;d=h+S|0;if(d>>>0<h>>>0){b=b+1|0}h=d+G|0;b=b+r|0;r=h;G=h>>>0<d>>>0?b+1|0:b;j=M(_^h,G^j,16);b=H;H=B;b=b+H|0;h=j;d=h+k|0;if(d>>>0<h>>>0){b=b+1|0}S=d;d=d^W;W=b;d=M(d,b^ka,63);b=B;k=b;f[i+32>>2]=d;f[i+36>>2]=b;i=t;b=pa+i|0;h=l;l=U;t=h+l|0;if(t>>>0<l>>>0){b=b+1|0}l=t+z|0;b=b+D|0;b=l>>>0<z>>>0?b+1|0:b;t=b;F=M(l^ea,b^F,32);_=h;h=B;b=O+h|0;z=F+Q|0;if(z>>>0<Q>>>0){b=b+1|0}D=i;i=b;Q=M(_^z,D^b,24);ka=Q;b=B;ea=b;D=b;b=t+ma|0;t=l+x|0;if(t>>>0<l>>>0){b=b+1|0}l=t+Q|0;b=b+D|0;ca=l;D=l>>>0<t>>>0?b+1|0:b;O=M(l^F,D^h,16);Q=B;b=i+Q|0;h=z+O|0;if(h>>>0<z>>>0){b=b+1|0}z=h;F=b;l=M(h^ka,b^ea,63);t=B;_=c;i=p;b=ja+i|0;h=s;c=e+h|0;if(c>>>0<e>>>0){b=b+1|0}c=c+u|0;b=b+T|0;b=c>>>0<u>>>0?b+1|0:b;e=b;K=M(_^c,b^K,32);u=h;h=B;b=E+h|0;s=w+K|0;if(s>>>0<w>>>0){b=b+1|0}p=i;i=b;w=M(u^s,p^b,24);u=w;b=B;E=b;p=b;b=e+ra|0;e=c+y|0;if(e>>>0<c>>>0){b=b+1|0}c=e+w|0;b=b+p|0;ra=c;p=c>>>0<e>>>0?b+1|0:b;ka=M(c^K,p^h,16);K=B;b=i+K|0;h=s+ka|0;if(h>>>0<s>>>0){b=b+1|0}s=h;e=b;y=M(h^u,b^E,63);h=B;i=q;b=ha+i|0;c=o;o=N+c|0;if(o>>>0<N>>>0){b=b+1|0}o=o+I|0;b=b+v|0;N=o;q=m;m=o>>>0<I>>>0?b+1|0:b;I=M(o^ia,q^m,32);u=c;c=B;b=L+c|0;o=I+C|0;if(o>>>0<C>>>0){b=b+1|0}q=i;i=b;w=M(u^o,q^b,24);u=w;b=B;v=b;q=b;E=I;b=m+ga|0;m=N+V|0;if(m>>>0<V>>>0){b=b+1|0}I=m;m=m+w|0;b=b+q|0;q=m;I=m>>>0<I>>>0?b+1|0:b;C=M(E^m,I^c,16);N=B;b=i+N|0;c=o+C|0;if(c>>>0<o>>>0){b=b+1|0}w=c;i=b;m=M(c^u,b^v,63);c=B;o=n;u=l;v=t;E=e;L=f[n+164>>2];V=C;b=t+G|0;e=l+r|0;if(e>>>0<r>>>0){b=b+1|0}l=e;e=e+f[n+160>>2]|0;b=b+L|0;C=e;l=e>>>0<l>>>0?b+1|0:b;e=M(V^e,l^N,32);r=u;u=B;b=u+E|0;s=e+s|0;if(s>>>0<e>>>0){b=b+1|0}t=s;s=b;G=M(r^t,b^v,24);b=B;r=b;N=b;b=l+da|0;l=C+J|0;if(l>>>0<J>>>0){b=b+1|0}J=l;l=l+G|0;b=b+N|0;L=l;N=l>>>0<J>>>0?b+1|0:b;T=M(e^l,u^N,16);b=B;l=b;f[o+120>>2]=T;f[o+124>>2]=b;b=b+s|0;e=t+T|0;if(e>>>0<t>>>0){b=b+1|0}t=e;s=b;e=M(e^G,b^r,63);o=B;r=y;J=h;G=j;u=r;b=D+ba|0;j=aa;y=j+ca|0;if(y>>>0<j>>>0){b=b+1|0}j=u+y|0;b=b+h|0;D=j;j=j>>>0<y>>>0?b+1|0:b;h=M(G^D,j^H,32);H=r;b=i;i=B;b=b+i|0;r=h+w|0;if(r>>>0<h>>>0){b=b+1|0}y=r;J=M(H^r,b^J,24);G=J;r=B;w=r;H=b;u=h;b=j+oa|0;h=fa;j=h+D|0;if(j>>>0<h>>>0){b=b+1|0}h=j+J|0;b=b+r|0;r=h;D=h>>>0<j>>>0?b+1|0:b;h=M(u^h,D^i,16);b=H;H=B;b=b+H|0;j=h+y|0;if(j>>>0<h>>>0){b=b+1|0}J=j;i=j^G;G=b;i=M(i,b^w,63);j=B;y=m;w=c;u=m;b=p+la|0;m=X;p=m+ra|0;if(p>>>0<m>>>0){b=b+1|0}m=u+p|0;b=b+c|0;v=m;c=m>>>0<p>>>0?b+1|0:b;u=M(m^O,c^Q,32);E=y;m=B;b=W+m|0;y=u+S|0;if(y>>>0<S>>>0){b=b+1|0}p=b;E=M(E^y,b^w,24);C=E;b=B;Q=b;w=b;O=u;b=c+qa|0;c=R;u=c+v|0;if(u>>>0<c>>>0){b=b+1|0}c=u+E|0;b=b+w|0;w=c;u=c>>>0<u>>>0?b+1|0:b;S=M(O^c,u^m,16);v=B;b=p+v|0;c=y+S|0;if(c>>>0<y>>>0){b=b+1|0}p=c;c=b;m=M(p^C,b^Q,63);y=B;E=d;C=k;b=k+na|0;d=Y;k=E+d|0;if(k>>>0<d>>>0){b=b+1|0}d=k+q|0;b=b+I|0;Q=d;d=d>>>0<q>>>0?b+1|0:b;K=M(Q^ka,d^K,32);k=c;c=B;b=F+c|0;q=z+K|0;if(q>>>0<z>>>0){b=b+1|0}I=b;z=M(q^E,b^C,24);E=z;b=B;C=b;F=f[n+204>>2];b=b+d|0;d=z+Q|0;if(d>>>0<z>>>0){b=b+1|0}z=d;d=d+f[n+200>>2]|0;b=b+F|0;b=d>>>0<z>>>0?b+1|0:b;z=d;d=d^K;K=b;Q=M(d,b^c,16);d=E;E=B;b=I+E|0;c=q+Q|0;if(c>>>0<q>>>0){b=b+1|0}q=c;I=b;C=M(d^c,b^C,63);b=B;F=b;c=b;O=h;b=N+Z|0;h=$;d=h+L|0;if(d>>>0<h>>>0){b=b+1|0}h=d+C|0;b=b+c|0;L=h;d=h>>>0<d>>>0?b+1|0:b;h=M(O^h,d^H,32);c=n;b=k;k=B;b=b+k|0;N=h+p|0;if(N>>>0<h>>>0){b=b+1|0}p=N;N=b;C=M(C^p,F^b,24);O=C;b=B;V=b;H=b;F=h;b=d+oa|0;h=fa;d=h+L|0;if(d>>>0<h>>>0){b=b+1|0}h=d+C|0;b=b+H|0;H=h;C=h>>>0<d>>>0?b+1|0:b;F=M(F^h,C^k,16);L=B;b=N+L|0;h=p+F|0;if(h>>>0<p>>>0){b=b+1|0}p=h;N=b;h=M(h^O,b^V,63);b=B;d=b;f[c+32>>2]=h;f[c+36>>2]=b;k=o;b=na+k|0;c=e;e=Y;o=c+e|0;if(o>>>0<e>>>0){b=b+1|0}e=o+r|0;b=b+D|0;D=e;e=e>>>0<r>>>0?b+1|0:b;r=M(D^S,e^v,32);v=c;c=B;b=I+c|0;o=q+r|0;if(o>>>0<q>>>0){b=b+1|0}q=k;k=b;I=M(v^o,q^b,24);v=I;b=B;O=b;q=b;S=r;b=e+la|0;e=X;r=e+D|0;if(r>>>0<e>>>0){b=b+1|0}e=r+I|0;b=b+q|0;V=e;q=e>>>0<r>>>0?b+1|0:b;S=M(S^e,q^c,16);r=B;b=k+r|0;c=o+S|0;if(c>>>0<o>>>0){b=b+1|0}I=c;D=b;o=M(c^v,b^O,63);k=B;e=j;b=qa+e|0;c=i;i=R;j=c+i|0;if(j>>>0<i>>>0){b=b+1|0}i=j+w|0;b=b+u|0;b=i>>>0<w>>>0?b+1|0:b;j=b;v=M(i^Q,b^E,32);u=c;w=B;b=s+w|0;c=t+v|0;if(c>>>0<t>>>0){b=b+1|0}t=c;s=e;e=b;c=M(u^c,s^b,24);b=B;s=c;u=b;Q=f[n+236>>2];E=v;b=j+b|0;c=c+i|0;if(c>>>0<i>>>0){b=b+1|0}i=c;c=c+f[n+232>>2]|0;b=b+Q|0;v=c;j=w;w=c>>>0<i>>>0?b+1|0:b;Q=M(E^c,j^w,16);i=s;s=B;b=e+s|0;c=t+Q|0;if(c>>>0<t>>>0){b=b+1|0}t=c;e=b;i=M(i^c,b^u,63);c=B;u=y;E=f[n+164>>2];b=y+K|0;j=m;m=z+j|0;if(m>>>0<z>>>0){b=b+1|0}y=m;m=m+f[n+160>>2]|0;b=b+E|0;E=m;m=m>>>0<y>>>0?b+1|0:b;K=M(E^T,m^l,32);z=j;j=B;b=G+j|0;y=K+J|0;if(y>>>0<J>>>0){b=b+1|0}l=b;J=M(z^y,b^u,24);G=J;b=B;u=b;z=b;O=K;b=m+ma|0;m=x;K=m+E|0;if(K>>>0<m>>>0){b=b+1|0}m=K+J|0;b=b+z|0;W=m;z=m>>>0<K>>>0?b+1|0:b;T=M(O^m,z^j,16);K=B;b=l+K|0;m=y+T|0;if(m>>>0<y>>>0){b=b+1|0}l=m;J=b;m=M(m^G,b^u,63);j=B;y=n;G=o;u=k;E=e;O=f[n+132>>2];b=k+C|0;e=o+H|0;if(e>>>0<H>>>0){b=b+1|0}o=e;e=e+f[n+128>>2]|0;b=b+O|0;C=e;o=e>>>0<o>>>0?b+1|0:b;e=M(e^T,o^K,32);b=E;E=B;b=b+E|0;t=e+t|0;if(t>>>0<e>>>0){b=b+1|0}k=t;t=b;G=M(k^G,b^u,24);b=B;H=b;K=b;b=o+ba|0;o=aa;u=o+C|0;if(u>>>0<o>>>0){b=b+1|0}o=u+G|0;b=b+K|0;ka=o;K=o>>>0<u>>>0?b+1|0:b;T=M(e^o,E^K,16);b=B;o=b;f[y+120>>2]=T;f[y+124>>2]=b;b=b+t|0;e=k+T|0;if(e>>>0<k>>>0){b=b+1|0}t=e;k=H;H=b;e=M(e^G,k^b,63);y=B;k=i;G=c;u=i;b=q+pa|0;i=U;q=i+V|0;if(q>>>0<i>>>0){b=b+1|0}i=u+q|0;b=b+c|0;b=i>>>0<q>>>0?b+1|0:b;q=b;E=M(i^F,b^L,32);u=k;k=B;b=J+k|0;c=l+E|0;if(c>>>0<l>>>0){b=b+1|0}l=c;J=b;c=M(u^c,b^G,24);b=B;G=c;u=b;L=f[n+156>>2];b=q+b|0;c=c+i|0;if(c>>>0<i>>>0){b=b+1|0}i=c;c=c+f[n+152>>2]|0;b=b+L|0;q=c;C=c^E;E=c>>>0<i>>>0?b+1|0:b;F=M(C,E^k,16);k=G;G=B;b=J+G|0;c=l+F|0;if(c>>>0<l>>>0){b=b+1|0}J=c;i=u;u=b;i=M(k^c,i^b,63);k=B;l=j;C=f[n+204>>2];b=j+w|0;c=m;m=v+c|0;if(m>>>0<v>>>0){b=b+1|0}j=m;m=j+f[n+200>>2]|0;b=b+C|0;b=m>>>0<j>>>0?b+1|0:b;j=b;v=M(m^S,b^r,32);C=c;r=B;b=N+r|0;c=p+v|0;if(c>>>0<p>>>0){b=b+1|0}p=c;w=l;l=b;c=M(C^c,w^b,24);b=B;N=c;w=b;L=f[n+148>>2];C=v;b=j+b|0;c=c+m|0;if(c>>>0<m>>>0){b=b+1|0}m=c;c=c+f[n+144>>2]|0;b=b+L|0;v=c;j=r;r=c>>>0<m>>>0?b+1|0:b;O=M(C^c,j^r,16);j=N;N=B;b=l+N|0;c=p+O|0;if(c>>>0<p>>>0){b=b+1|0}l=b;m=M(j^c,b^w,63);j=B;p=h;w=d;C=f[n+196>>2];b=d+z|0;d=h+W|0;if(d>>>0<h>>>0){b=b+1|0}h=d+f[n+192>>2]|0;b=b+C|0;L=h;d=h>>>0<d>>>0?b+1|0:b;C=M(h^Q,d^s,32);z=p;s=B;b=D+s|0;h=I+C|0;if(h>>>0<I>>>0){b=b+1|0}p=h;h=M(z^h,b^w,24);z=h;D=b;V=f[n+220>>2];Q=V;S=C;I=B;b=I+d|0;d=h+L|0;if(d>>>0<h>>>0){b=b+1|0}w=f[n+216>>2];h=w+d|0;b=b+Q|0;C=h;L=s;s=h>>>0<d>>>0?b+1|0:b;d=M(S^h,L^s,16);L=z;z=B;b=z+D|0;p=d+p|0;if(p>>>0<d>>>0){b=b+1|0}h=I;I=b;h=M(L^p,h^b,63);b=B;D=h;L=b;Q=f[n+236>>2];S=F;b=K+b|0;K=h+ka|0;if(K>>>0<h>>>0){b=b+1|0}h=K+f[n+232>>2]|0;b=b+Q|0;F=h;K=h>>>0<K>>>0?b+1|0:b;Q=M(S^h,K^G,32);S=D;D=B;b=l+D|0;h=c+Q|0;if(h>>>0<c>>>0){b=b+1|0}l=h;G=b;h=M(S^h,b^L,24);b=B;c=n;L=h;S=b;b=K+b|0;K=h+F|0;if(K>>>0<h>>>0){b=b+1|0}h=w+K|0;b=b+V|0;V=h;K=h>>>0<w>>>0?b+1|0:b;Q=M(h^Q,K^D,16);D=B;b=G+D|0;h=l+Q|0;if(h>>>0<l>>>0){b=b+1|0}G=h;w=b;h=M(h^L,b^S,63);b=B;l=b;f[c+32>>2]=h;f[c+36>>2]=b;L=y;b=y+ba|0;c=e;e=aa;y=c+e|0;if(y>>>0<e>>>0){b=b+1|0}e=q+y|0;b=b+E|0;E=e;e=e>>>0<q>>>0?b+1|0:b;N=M(E^O,e^N,32);F=c;c=B;b=I+c|0;y=p+N|0;if(y>>>0<p>>>0){b=b+1|0}q=b;I=M(F^y,b^L,24);S=I;b=B;F=b;p=b;L=I;b=e+qa|0;e=R;I=e+E|0;if(I>>>0<e>>>0){b=b+1|0}e=L+I|0;b=b+p|0;O=e;p=e>>>0<I>>>0?b+1|0:b;L=M(e^N,p^c,16);I=B;b=q+I|0;c=y+L|0;if(c>>>0<y>>>0){b=b+1|0}N=c;E=b;y=M(c^S,b^F,63);c=B;q=k;F=d;b=k+Z|0;e=i;i=$;d=e+i|0;if(d>>>0<i>>>0){b=b+1|0}i=d+v|0;b=b+r|0;b=i>>>0<v>>>0?b+1|0:b;v=i;i=b;r=M(F^v,b^z,32);z=e;e=B;b=H+e|0;d=t+r|0;if(d>>>0<t>>>0){b=b+1|0}k=b;t=M(z^d,b^q,24);z=t;b=B;H=b;q=b;F=t;b=i+na|0;i=Y;t=i+v|0;if(t>>>0<i>>>0){b=b+1|0}i=F+t|0;b=b+q|0;b=i>>>0<t>>>0?b+1|0:b;t=i;i=i^r;r=b;F=M(i,b^e,16);i=z;z=B;b=k+z|0;e=d+F|0;if(e>>>0<d>>>0){b=b+1|0}k=e;q=b;i=M(i^e,b^H,63);e=B;H=j;v=f[n+156>>2];b=j+s|0;d=m;m=C+d|0;if(m>>>0<C>>>0){b=b+1|0}j=m;m=j+f[n+152>>2]|0;b=b+v|0;b=m>>>0<j>>>0?b+1|0:b;j=o;o=b;v=M(m^T,j^b,32);C=d;j=B;b=u+j|0;d=v+J|0;if(d>>>0<J>>>0){b=b+1|0}s=d;J=H;H=b;d=M(C^d,J^b,24);b=B;J=d;u=b;S=f[n+204>>2];b=o+b|0;d=d+m|0;if(d>>>0<m>>>0){b=b+1|0}m=d;d=d+f[n+200>>2]|0;b=b+S|0;W=d;o=d^v;v=d>>>0<m>>>0?b+1|0:b;C=M(o,v^j,16);j=B;b=H+j|0;d=s+C|0;if(d>>>0<s>>>0){b=b+1|0}s=d;H=b;d=M(d^J,b^u,63);m=B;o=n;J=y;u=c;S=y;b=K+oa|0;y=fa;K=y+V|0;if(K>>>0<y>>>0){b=b+1|0}y=S+K|0;b=b+c|0;c=y;y=j;j=c>>>0<K>>>0?b+1|0:b;K=M(c^C,y^j,32);C=B;b=q+C|0;y=k+K|0;if(y>>>0<k>>>0){b=b+1|0}k=b;J=M(y^J,b^u,24);q=B;b=j+q|0;j=c+J|0;if(j>>>0<c>>>0){b=b+1|0}c=j;j=f[n+128>>2];c=c+j|0;b=f[n+132>>2]+b|0;V=c;u=K^c;K=c>>>0<j>>>0?b+1|0:b;S=M(u,C^K,16);b=B;j=b;f[o+120>>2]=S;f[o+124>>2]=b;o=J;b=b+k|0;c=y+S|0;if(c>>>0<y>>>0){b=b+1|0}J=c;u=b;o=M(o^c,b^q,63);y=B;k=e;c=i;C=c;b=p+la|0;i=X;q=i+O|0;if(q>>>0<i>>>0){b=b+1|0}i=C+q|0;b=b+e|0;e=i;i=e>>>0<q>>>0?b+1|0:b;D=M(e^Q,i^D,32);C=c;q=B;b=H+q|0;c=s+D|0;if(c>>>0<s>>>0){b=b+1|0}s=c;p=k;k=b;c=M(C^c,p^b,24);b=B;p=c;H=b;T=f[n+164>>2];O=D;b=i+b|0;c=c+e|0;if(c>>>0<e>>>0){b=b+1|0}e=c;c=c+f[n+160>>2]|0;b=b+T|0;D=c;C=c>>>0<e>>>0?b+1|0:b;e=M(O^c,C^q,16);i=B;b=k+i|0;c=e+s|0;if(c>>>0<s>>>0){b=b+1|0}s=c;c=c^p;p=b;k=M(c,b^H,63);q=B;H=m;T=f[n+196>>2];O=L;b=m+r|0;c=d;d=t+c|0;if(d>>>0<t>>>0){b=b+1|0}m=d;d=d+f[n+192>>2]|0;b=b+T|0;L=d;d=d>>>0<m>>>0?b+1|0:b;I=M(O^L,d^I,32);r=c;c=B;b=w+c|0;m=G+I|0;if(m>>>0<G>>>0){b=b+1|0}t=b;H=M(r^m,b^H,24);G=H;b=B;w=b;r=b;O=I;b=d+pa|0;d=U;I=d+L|0;if(I>>>0<d>>>0){b=b+1|0}d=I+H|0;b=b+r|0;r=d;I=d>>>0<I>>>0?b+1|0:b;T=M(O^d,I^c,16);H=B;b=t+H|0;c=m+T|0;if(c>>>0<m>>>0){b=b+1|0}t=c;c=b;d=M(t^G,b^w,63);m=B;G=h;w=l;L=f[n+148>>2];b=l+v|0;l=h+W|0;if(l>>>0<h>>>0){b=b+1|0}h=l+f[n+144>>2]|0;b=b+L|0;L=h;h=h>>>0<l>>>0?b+1|0:b;v=M(L^F,h^z,32);l=c;c=B;b=E+c|0;z=v+N|0;if(z>>>0<N>>>0){b=b+1|0}N=b;w=M(z^G,b^w,24);E=w;b=B;F=b;G=b;O=w;b=h+ma|0;h=x;w=h+L|0;if(w>>>0<h>>>0){b=b+1|0}h=O+w|0;b=b+G|0;G=h;w=h>>>0<w>>>0?b+1|0:b;Q=M(h^v,w^c,16);v=B;b=N+v|0;h=z+Q|0;if(h>>>0<z>>>0){b=b+1|0}z=h;N=b;E=M(h^E,b^F,63);b=B;L=b;h=b;O=e;b=K+pa|0;c=U;e=c+V|0;if(e>>>0<c>>>0){b=b+1|0}c=e+E|0;b=b+h|0;F=c;e=c>>>0<e>>>0?b+1|0:b;h=M(O^c,e^i,32);c=n;i=B;b=i+l|0;t=h+t|0;if(t>>>0<h>>>0){b=b+1|0}l=t;t=b;E=M(E^l,L^b,24);L=E;b=B;O=b;K=b;W=h;b=e+la|0;h=X;e=h+F|0;if(e>>>0<h>>>0){b=b+1|0}h=e+E|0;b=b+K|0;V=h;e=h>>>0<e>>>0?b+1|0:b;F=M(W^h,e^i,16);K=B;b=t+K|0;h=l+F|0;if(h>>>0<l>>>0){b=b+1|0}E=h;h=h^L;L=b;i=M(h,b^O,63);b=B;l=b;f[c+32>>2]=i;f[c+36>>2]=b;c=y;b=qa+c|0;h=o;o=R;y=h+o|0;if(y>>>0<o>>>0){b=b+1|0}o=y+D|0;b=b+C|0;b=o>>>0<D>>>0?b+1|0:b;y=b;D=M(o^T,b^H,32);C=h;t=B;b=N+t|0;h=z+D|0;if(h>>>0<z>>>0){b=b+1|0}z=h;H=c;c=b;h=M(C^h,H^b,24);b=B;N=h;H=b;T=f[n+204>>2];O=D;b=y+b|0;h=h+o|0;if(h>>>0<o>>>0){b=b+1|0}o=h;h=h+f[n+200>>2]|0;b=b+T|0;D=h;C=t;t=h>>>0<o>>>0?b+1|0:b;O=M(O^h,C^t,16);o=N;N=B;b=c+N|0;h=z+O|0;if(h>>>0<z>>>0){b=b+1|0}z=h;c=H;H=b;T=M(o^h,c^b,63);h=B;C=f[n+220>>2];o=q;b=I+o|0;c=k;y=r+c|0;if(y>>>0<r>>>0){b=b+1|0}k=y;y=k+f[n+216>>2]|0;b=b+C|0;b=y>>>0<k>>>0?b+1|0:b;k=b;v=M(y^Q,b^v,32);I=c;q=B;b=u+q|0;c=v+J|0;if(c>>>0<J>>>0){b=b+1|0}r=c;J=o;o=b;c=M(I^c,J^b,24);b=B;I=c;J=b;C=f[n+156>>2];b=k+b|0;c=c+y|0;if(c>>>0<y>>>0){b=b+1|0}y=c;c=c+f[n+152>>2]|0;b=b+C|0;Q=c;u=c>>>0<y>>>0?b+1|0:b;W=M(c^v,u^q,16);C=I;I=B;b=o+I|0;c=r+W|0;if(c>>>0<r>>>0){b=b+1|0}k=c;q=b;o=M(C^c,b^J,63);y=B;r=m;J=f[n+132>>2];b=m+w|0;c=d;d=G+c|0;if(d>>>0<G>>>0){b=b+1|0}m=d;d=d+f[n+128>>2]|0;b=b+J|0;b=d>>>0<m>>>0?b+1|0:b;m=b;G=M(d^S,b^j,32);J=c;j=B;b=p+j|0;c=s+G|0;if(c>>>0<s>>>0){b=b+1|0}s=c;p=b;c=M(J^c,b^r,24);b=B;r=c;J=b;v=f[n+196>>2];C=G;b=m+b|0;c=c+d|0;if(c>>>0<d>>>0){b=b+1|0}d=c;c=c+f[n+192>>2]|0;b=b+v|0;G=c;w=c>>>0<d>>>0?b+1|0:b;v=M(C^c,w^j,16);m=B;b=p+m|0;c=s+v|0;if(c>>>0<s>>>0){b=b+1|0}s=c;p=b;d=M(c^r,b^J,63);c=B;j=h;b=e+Z|0;e=$;r=e+V|0;if(r>>>0<e>>>0){b=b+1|0}e=r+T|0;b=b+j|0;j=e;b=e>>>0<r>>>0?b+1|0:b;e=M(e^v,b^m,32);m=n;J=e;v=j;C=b;r=B;b=q+r|0;e=e+k|0;if(e>>>0<k>>>0){b=b+1|0}q=b;h=M(e^T,b^h,24);k=n;j=f[n+144>>2];V=J;J=B;b=J+C|0;C=h+v|0;if(C>>>0<h>>>0){b=b+1|0}S=j+C|0;C=f[k+148>>2];b=C+b|0;v=S;k=r;r=v>>>0<j>>>0?b+1|0:b;V=M(V^v,k^r,16);b=B;k=b;f[m+120>>2]=V;f[m+124>>2]=b;m=h;b=b+q|0;h=e+V|0;if(h>>>0<e>>>0){b=b+1|0}S=h;e=b;m=M(m^h,b^J,63);q=B;J=y;T=f[n+236>>2];b=t+y|0;h=o;o=D+h|0;if(o>>>0<D>>>0){b=b+1|0}y=o;o=o+f[n+232>>2]|0;b=b+T|0;D=o;o=o>>>0<y>>>0?b+1|0:b;K=M(D^F,o^K,32);F=h;h=B;b=p+h|0;y=s+K|0;if(y>>>0<s>>>0){b=b+1|0}t=b;p=M(F^y,b^J,24);J=p;b=B;T=b;s=b;F=p;b=o+ba|0;o=aa;p=o+D|0;if(p>>>0<o>>>0){b=b+1|0}o=F+p|0;b=b+s|0;b=o>>>0<p>>>0?b+1|0:b;p=o;s=o^K;K=b;o=M(s,b^h,16);s=J;D=B;b=t+D|0;h=o+y|0;if(h>>>0<y>>>0){b=b+1|0}J=h;t=T;T=b;y=M(s^h,t^b,63);t=B;s=c;h=d;F=d;b=u+na|0;d=Y;u=d+Q|0;if(u>>>0<d>>>0){b=b+1|0}d=F+u|0;b=b+c|0;c=d;d=c>>>0<u>>>0?b+1|0:b;O=M(c^O,d^N,32);F=h;N=B;b=L+N|0;h=E+O|0;if(h>>>0<E>>>0){b=b+1|0}u=h;h=M(F^h,b^s,24);E=h;L=b;b=f[n+164>>2];Q=b;F=b;s=B;b=d+s|0;h=c+h|0;if(h>>>0<c>>>0){b=b+1|0}ra=f[n+160>>2];c=h;h=ra+c|0;b=b+F|0;F=h;d=N;N=h>>>0<c>>>0?b+1|0:b;c=M(h^O,d^N,16);O=E;E=B;b=E+L|0;d=c+u|0;if(d>>>0<c>>>0){b=b+1|0}u=d;h=b;d=M(O^d,b^s,63);s=B;L=i;O=l;b=l+ma|0;i=x;l=L+i|0;if(l>>>0<i>>>0){b=b+1|0}i=l+G|0;b=b+w|0;w=i;i=i>>>0<G>>>0?b+1|0:b;G=M(w^W,i^I,32);l=h;h=B;b=H+h|0;I=z+G|0;if(I>>>0<z>>>0){b=b+1|0}z=b;L=M(I^L,b^O,24);O=L;b=B;W=b;H=b;_=G;b=i+oa|0;i=fa;G=i+w|0;if(G>>>0<i>>>0){b=b+1|0}i=G+L|0;b=b+H|0;H=i;G=i>>>0<G>>>0?b+1|0:b;L=M(_^i,G^h,16);w=B;b=z+w|0;h=I+L|0;if(h>>>0<I>>>0){b=b+1|0}I=h;z=b;i=M(h^O,b^W,63);b=B;O=b;h=b;_=o;b=r+ma|0;x=v+x|0;if(x>>>0<v>>>0){b=b+1|0}o=x;x=o+i|0;b=b+h|0;W=x;o=x>>>0<o>>>0?b+1|0:b;h=M(_^x,o^D,32);b=l;l=B;b=b+l|0;r=h+u|0;if(r>>>0<h>>>0){b=b+1|0}D=b;x=M(i^r,O^b,24);b=B;i=n;u=x;v=b;_=h;b=o+b|0;h=x+W|0;if(h>>>0<x>>>0){b=b+1|0}x=h+j|0;b=b+C|0;O=x;j=l;l=x>>>0<h>>>0?b+1|0:b;C=M(_^x,j^l,16);h=u;u=B;b=D+u|0;x=r+C|0;if(x>>>0<r>>>0){b=b+1|0}r=x;D=b;x=M(h^r,b^v,63);b=B;o=b;f[i+32>>2]=x;f[i+36>>2]=b;j=f[n+196>>2];W=c;i=q;b=K+i|0;h=m;c=p+h|0;if(c>>>0<p>>>0){b=b+1|0}m=c;c=c+f[n+192>>2]|0;b=b+j|0;v=c;c=c>>>0<m>>>0?b+1|0:b;K=M(W^v,c^E,32);q=h;m=B;b=z+m|0;h=K+I|0;if(h>>>0<I>>>0){b=b+1|0}j=h;p=i;i=b;h=M(q^h,p^b,24);b=B;q=h;p=b;b=c+b|0;c=h+v|0;if(c>>>0<h>>>0){b=b+1|0}h=c+ra|0;b=b+Q|0;W=h;I=h>>>0<c>>>0?b+1|0:b;Q=M(h^K,I^m,16);c=q;q=B;b=i+q|0;h=j+Q|0;if(h>>>0<j>>>0){b=b+1|0}z=h;i=b;m=M(c^h,b^p,63);h=B;p=t;b=ba+p|0;j=y;c=aa;y=j+c|0;if(y>>>0<c>>>0){b=b+1|0}c=y+F|0;b=b+N|0;N=c;y=c>>>0<F>>>0?b+1|0:b;c=M(c^L,y^w,32);E=j;b=e;e=B;b=b+e|0;t=c+S|0;if(t>>>0<c>>>0){b=b+1|0}j=t;t=b;K=M(E^j,b^p,24);w=K;b=B;v=b;p=b;E=c;b=y+pa|0;c=U;y=c+N|0;if(y>>>0<c>>>0){b=b+1|0}c=y+K|0;b=b+p|0;p=c;N=c>>>0<y>>>0?b+1|0:b;L=M(E^c,N^e,16);K=B;b=t+K|0;c=j+L|0;if(c>>>0<j>>>0){b=b+1|0}t=c;e=b;c=M(c^w,b^v,63);j=B;y=d;w=s;b=s+na|0;d=Y;s=y+d|0;if(s>>>0<d>>>0){b=b+1|0}d=s+H|0;b=b+G|0;v=d;d=d>>>0<H>>>0?b+1|0:b;G=M(v^V,d^k,32);H=y;y=B;b=T+y|0;k=G+J|0;if(k>>>0<J>>>0){b=b+1|0}s=b;J=M(H^k,b^w,24);w=J;b=B;S=b;H=b;E=w;b=d+oa|0;d=fa;J=d+v|0;if(J>>>0<d>>>0){b=b+1|0}d=E+J|0;b=b+H|0;F=d;H=d>>>0<J>>>0?b+1|0:b;E=M(d^G,H^y,16);J=B;b=s+J|0;d=k+E|0;if(d>>>0<k>>>0){b=b+1|0}s=d;G=b;d=M(d^w,b^S,63);y=B;k=n;w=m;v=h;S=m;b=l+la|0;m=X;l=m+O|0;if(l>>>0<m>>>0){b=b+1|0}m=S+l|0;b=b+h|0;b=m>>>0<l>>>0?b+1|0:b;l=b;h=M(m^E,b^J,32);E=B;b=E+e|0;t=h+t|0;if(t>>>0<h>>>0){b=b+1|0}S=t;t=b;v=M(S^w,b^v,24);J=B;b=l+J|0;e=m+v|0;if(e>>>0<m>>>0){b=b+1|0}m=f[n+216>>2];e=m+e|0;b=f[n+220>>2]+b|0;O=e;w=e>>>0<m>>>0?b+1|0:b;e=M(h^e,E^w,16);b=B;m=b;f[k+120>>2]=e;f[k+124>>2]=b;l=v;b=b+t|0;k=e+S|0;if(k>>>0<e>>>0){b=b+1|0}v=k;h=J;J=b;h=M(l^k,h^b,63);k=B;l=c;t=j;E=f[n+204>>2];b=j+I|0;j=c+W|0;if(j>>>0<c>>>0){b=b+1|0}c=j+f[n+200>>2]|0;b=b+E|0;E=c;c=c>>>0<j>>>0?b+1|0:b;I=M(E^C,c^u,32);u=l;j=B;b=G+j|0;l=s+I|0;if(l>>>0<s>>>0){b=b+1|0}s=t;t=b;G=M(u^l,s^b,24);u=G;b=B;C=b;s=b;S=I;b=c+qa|0;c=R;I=c+E|0;if(I>>>0<c>>>0){b=b+1|0}c=I+G|0;b=b+s|0;V=c;s=c>>>0<I>>>0?b+1|0:b;S=M(S^c,s^j,16);I=B;b=t+I|0;c=l+S|0;if(c>>>0<l>>>0){b=b+1|0}G=c;c=b;j=M(G^u,b^C,63);l=B;t=d;u=y;b=f[n+156>>2];E=b;C=b;b=y+N|0;d=d+p|0;if(d>>>0<p>>>0){b=b+1|0}W=f[n+152>>2];y=d;d=W+d|0;b=b+C|0;C=d;d=d>>>0<y>>>0?b+1|0:b;N=M(C^Q,d^q,32);y=B;b=D+y|0;q=r+N|0;if(q>>>0<r>>>0){b=b+1|0}p=q^t;t=b;r=M(p,b^u,24);D=r;b=B;u=b;p=b;Q=r;b=d+Z|0;d=$;r=d+C|0;if(r>>>0<d>>>0){b=b+1|0}d=Q+r|0;b=b+p|0;p=d;r=d>>>0<r>>>0?b+1|0:b;T=M(d^N,r^y,16);N=B;b=t+N|0;d=q+T|0;if(d>>>0<q>>>0){b=b+1|0}q=d;t=b;d=M(d^D,b^u,63);y=B;D=x;u=o;C=i;i=f[n+236>>2];Q=L;b=o+H|0;o=x+F|0;if(o>>>0<x>>>0){b=b+1|0}x=o+f[n+232>>2]|0;b=b+i|0;L=x;o=x>>>0<o>>>0?b+1|0:b;i=M(Q^x,o^K,32);H=B;b=H+C|0;z=i+z|0;if(z>>>0<i>>>0){b=b+1|0}x=M(z^D,b^u,24);D=x;u=b;Q=f[n+132>>2];F=Q;_=i;K=B;b=K+o|0;o=x+L|0;if(o>>>0<x>>>0){b=b+1|0}i=f[n+128>>2];x=i+o|0;b=b+F|0;C=x;L=H;H=x>>>0<o>>>0?b+1|0:b;o=M(_^x,L^H,16);x=D;D=B;b=D+u|0;z=o+z|0;if(z>>>0<o>>>0){b=b+1|0}u=K;K=b;x=M(x^z,u^b,63);b=B;u=x;L=b;b=w+b|0;w=x+O|0;if(w>>>0<x>>>0){b=b+1|0}x=i+w|0;b=b+Q|0;Q=x;i=x>>>0<i>>>0?b+1|0:b;S=M(x^S,i^I,32);x=n;I=B;b=t+I|0;t=q+S|0;if(t>>>0<q>>>0){b=b+1|0}q=t;t=b;u=M(q^u,b^L,24);L=u;b=B;F=b;w=b;O=u;b=i+na|0;i=Y;u=i+Q|0;if(u>>>0<i>>>0){b=b+1|0}i=O+u|0;b=b+w|0;w=i;O=I;I=i>>>0<u>>>0?b+1|0:b;Q=M(i^S,O^I,16);O=L;u=B;b=t+u|0;i=q+Q|0;if(i>>>0<q>>>0){b=b+1|0}L=i;S=b;i=M(O^i,b^F,63);b=B;q=b;f[x+32>>2]=i;f[x+36>>2]=b;x=h;t=k;F=f[n+148>>2];b=k+s|0;k=h+V|0;if(k>>>0<h>>>0){b=b+1|0}h=k+f[n+144>>2]|0;b=b+F|0;F=h;h=h>>>0<k>>>0?b+1|0:b;T=M(F^T,h^N,32);N=x;k=B;b=K+k|0;x=z+T|0;if(x>>>0<z>>>0){b=b+1|0}s=x;x=t;t=b;x=M(N^s,x^b,24);b=B;z=x;N=b;b=h+b|0;h=x+F|0;if(h>>>0<x>>>0){b=b+1|0}x=h+W|0;b=b+E|0;O=x;K=x>>>0<h>>>0?b+1|0:b;E=M(x^T,K^k,16);k=z;z=B;b=t+z|0;x=s+E|0;if(x>>>0<s>>>0){b=b+1|0}s=x;h=N;N=b;k=M(k^s,h^b,63);t=B;x=j;T=f[n+164>>2];F=o;h=l;b=r+h|0;o=j+p|0;if(o>>>0<p>>>0){b=b+1|0}j=o;o=j+f[n+160>>2]|0;b=b+T|0;r=o;o=o>>>0<j>>>0?b+1|0:b;p=M(F^r,o^D,32);D=x;x=B;b=J+x|0;j=p+v|0;if(j>>>0<v>>>0){b=b+1|0}l=h;h=b;D=M(D^j,l^b,24);J=D;b=B;v=b;l=b;F=p;b=o+oa|0;o=fa;p=o+r|0;if(p>>>0<o>>>0){b=b+1|0}o=p+D|0;b=b+l|0;V=o;l=o>>>0<p>>>0?b+1|0:b;F=M(F^o,l^x,16);p=B;b=h+p|0;x=j+F|0;if(x>>>0<j>>>0){b=b+1|0}j=x;r=b;x=M(j^J,b^v,63);o=B;D=d;J=y;v=e;b=y+pa|0;h=U;e=h+d|0;if(e>>>0<h>>>0){b=b+1|0}h=e+C|0;b=b+H|0;H=h;e=h>>>0<C>>>0?b+1|0:b;h=M(v^h,e^m,32);b=c;c=B;b=b+c|0;m=h+G|0;if(m>>>0<h>>>0){b=b+1|0}d=m;m=b;D=M(d^D,b^J,24);J=D;b=B;G=b;y=b;C=h;b=e+ba|0;h=aa;e=h+H|0;if(e>>>0<h>>>0){b=b+1|0}h=e+D|0;b=b+y|0;H=h;D=h>>>0<e>>>0?b+1|0:b;v=M(C^h,D^c,16);c=J;y=B;b=m+y|0;h=d+v|0;if(h>>>0<d>>>0){b=b+1|0}J=h;e=b;d=M(c^h,b^G,63);h=B;m=k;G=t;c=n;C=f[c+196>>2];b=t+I|0;k=k+w|0;if(k>>>0<w>>>0){b=b+1|0}t=k;k=k+f[c+192>>2]|0;b=b+C|0;b=k>>>0<t>>>0?b+1|0:b;t=y;y=b;I=M(k^v,t^b,32);C=m;w=B;b=r+w|0;m=j+I|0;if(m>>>0<j>>>0){b=b+1|0}j=m;t=b;G=M(C^j,b^G,24);r=B;b=y+r|0;m=k+G|0;if(m>>>0<k>>>0){b=b+1|0}y=f[n+200>>2];m=y+m|0;b=f[n+204>>2]+b|0;W=m;k=m>>>0<y>>>0?b+1|0:b;T=M(I^m,w^k,16);b=B;m=b;f[c+120>>2]=T;f[c+124>>2]=b;b=b+t|0;c=j+T|0;if(c>>>0<j>>>0){b=b+1|0}t=c;j=r;r=b;c=M(c^G,j^b,63);j=B;y=x;I=o;G=e;ma=f[n+212>>2];e=ma;b=o+K|0;o=x+O|0;if(o>>>0<x>>>0){b=b+1|0}w=f[n+208>>2];x=w+o|0;b=b+e|0;v=x;o=x>>>0<o>>>0?b+1|0:b;e=M(x^Q,o^u,32);u=y;y=B;b=y+G|0;K=e+J|0;if(K>>>0<e>>>0){b=b+1|0}J=I;I=b;x=M(u^K,J^b,24);b=B;J=x;G=b;C=f[n+220>>2];O=e;b=o+b|0;e=v+x|0;if(e>>>0<x>>>0){b=b+1|0}x=e+f[n+216>>2]|0;b=b+C|0;Q=x;u=x>>>0<e>>>0?b+1|0:b;e=M(O^x,u^y,16);o=J;J=B;b=I+J|0;x=e+K|0;if(x>>>0<K>>>0){b=b+1|0}I=x;K=b;o=M(o^x,b^G,63);y=B;x=d;G=h;C=d;b=l+Z|0;d=$;l=d+V|0;if(l>>>0<d>>>0){b=b+1|0}d=C+l|0;b=b+h|0;C=d;h=d>>>0<l>>>0?b+1|0:b;E=M(d^E,h^z,32);v=x;d=B;b=S+d|0;x=E+L|0;if(x>>>0<L>>>0){b=b+1|0}l=x;z=b;x=M(v^l,b^G,24);b=B;G=x;v=b;S=f[n+236>>2];L=E;b=h+b|0;h=x+C|0;if(h>>>0<x>>>0){b=b+1|0}x=h+f[n+232>>2]|0;b=b+S|0;E=x;C=x>>>0<h>>>0?b+1|0:b;L=M(L^x,C^d,16);d=G;G=B;b=z+G|0;x=l+L|0;if(x>>>0<l>>>0){b=b+1|0}h=x;l=b;x=M(d^h,b^v,63);d=B;z=i;v=q;b=q+qa|0;i=R;q=z+i|0;if(q>>>0<i>>>0){b=b+1|0}i=q+H|0;b=b+D|0;D=i;i=i>>>0<H>>>0?b+1|0:b;H=M(D^F,i^p,32);q=B;b=N+q|0;p=s+H|0;if(p>>>0<s>>>0){b=b+1|0}s=p;p=b;N=M(s^z,b^v,24);S=N;b=B;F=b;z=b;v=N;b=i+la|0;i=X;N=i+D|0;if(N>>>0<i>>>0){b=b+1|0}i=v+N|0;b=b+z|0;O=i;i=i>>>0<N>>>0?b+1|0:b;v=M(O^H,i^q,16);q=B;b=p+q|0;p=s+v|0;if(p>>>0<s>>>0){b=b+1|0}s=p;p=b;N=M(s^S,b^F,63);b=B;H=b;z=b;D=e;b=k+qa|0;e=R+W|0;if(e>>>0<R>>>0){b=b+1|0}R=e+N|0;b=b+z|0;S=R;e=R>>>0<e>>>0?b+1|0:b;D=M(D^R,e^J,32);k=B;b=l+k|0;R=h+D|0;if(R>>>0<h>>>0){b=b+1|0}l=R;z=b;R=M(N^l,H^b,24);b=B;h=n;N=R;H=b;b=e+b|0;e=R+S|0;if(e>>>0<R>>>0){b=b+1|0}R=e+w|0;b=b+ma|0;S=R;e=k;k=R>>>0<w>>>0?b+1|0:b;J=M(R^D,e^k,16);e=N;N=B;b=z+N|0;R=l+J|0;if(R>>>0<l>>>0){b=b+1|0}l=R;z=b;R=M(e^l,b^H,63);b=B;e=b;f[h+32>>2]=R;f[h+36>>2]=b;h=c;H=j;D=f[n+164>>2];b=j+u|0;j=c+Q|0;if(j>>>0<c>>>0){b=b+1|0}c=j+f[n+160>>2]|0;b=b+D|0;w=c;c=c>>>0<j>>>0?b+1|0:b;G=M(w^L,c^G,32);u=h;j=B;b=p+j|0;h=s+G|0;if(h>>>0<s>>>0){b=b+1|0}s=h;p=b;h=M(u^h,b^H,24);b=B;H=h;D=b;L=f[n+196>>2];b=c+b|0;c=h+w|0;if(c>>>0<h>>>0){b=b+1|0}h=c+f[n+192>>2]|0;b=b+L|0;L=h;u=j;j=h>>>0<c>>>0?b+1|0:b;G=M(h^G,u^j,16);Q=B;b=p+Q|0;h=s+G|0;if(h>>>0<s>>>0){b=b+1|0}s=h;p=b;D=M(h^H,b^D,63);h=B;H=y;w=f[n+204>>2];b=y+C|0;c=o;o=E+c|0;if(o>>>0<E>>>0){b=b+1|0}y=o;o=o+f[n+200>>2]|0;b=b+w|0;u=o;o=o>>>0<y>>>0?b+1|0:b;w=M(u^v,o^q,32);E=c;c=B;b=r+c|0;y=t+w|0;if(y>>>0<t>>>0){b=b+1|0}q=b;r=M(E^y,b^H,24);H=r;b=B;v=b;t=b;b=o+la|0;o=u+X|0;if(o>>>0<X>>>0){b=b+1|0}X=o+r|0;b=b+t|0;E=X;o=E>>>0<o>>>0?b+1|0:b;w=M(E^w,o^c,16);t=B;b=q+t|0;X=y+w|0;if(X>>>0<y>>>0){b=b+1|0}y=X;q=b;X=M(y^H,b^v,63);c=B;r=x;H=d;u=f[n+236>>2];b=d+i|0;i=r+O|0;if(i>>>0<r>>>0){b=b+1|0}x=i+f[n+232>>2]|0;b=b+u|0;v=x;x=x>>>0<i>>>0?b+1|0:b;u=M(v^T,x^m,32);i=B;b=K+i|0;d=u+I|0;if(d>>>0<I>>>0){b=b+1|0}m=b;I=M(d^r,b^H,24);H=I;b=B;C=b;r=b;b=x+pa|0;x=v+U|0;if(x>>>0<U>>>0){b=b+1|0}U=x+I|0;b=b+r|0;r=U;I=r>>>0<x>>>0?b+1|0:b;K=M(r^u,I^i,16);u=B;b=m+u|0;U=d+K|0;if(U>>>0<d>>>0){b=b+1|0}d=U;m=b;x=M(d^H,b^C,63);i=B;U=n;H=h;b=k+na|0;k=S+Y|0;if(k>>>0<Y>>>0){b=b+1|0}Y=k+D|0;b=b+H|0;b=Y>>>0<k>>>0?b+1|0:b;k=Z;Z=b;b=k+b|0;k=Y+$|0;if(k>>>0<Y>>>0){b=b+1|0}$=b;Z=M(K^Y,u^Z,32);H=B;b=q+H|0;Y=y+Z|0;if(Y>>>0<y>>>0){b=b+1|0}q=Y;u=h;h=b;Y=M(q^D,u^b,24);K=B;b=K+$|0;y=k+Y|0;if(y>>>0<Y>>>0){b=b+1|0}f[U>>2]=y;f[U+4>>2]=b;$=b;U=M(y^Z,b^H,16);b=B;f[n+120>>2]=U;f[n+124>>2]=b;b=h+b|0;h=q+U|0;if(h>>>0<U>>>0){b=b+1|0}f[n+80>>2]=h;f[n+84>>2]=b;f[n+40>>2]=M(h^Y,b^K,63);f[n+44>>2]=B;Y=n;h=c;Z=f[n+132>>2];b=c+j|0;U=X;c=U+L|0;if(c>>>0<U>>>0){b=b+1|0}X=c+f[n+128>>2]|0;b=b+Z|0;b=X>>>0<c>>>0?b+1|0:b;c=b;j=M(X^J,b^N,32);k=U;Z=B;b=m+Z|0;U=d+j|0;if(U>>>0<d>>>0){b=b+1|0}d=U;U=b;h=M(k^d,b^h,24);m=B;b=c+m|0;c=h+X|0;if(c>>>0<X>>>0){b=b+1|0}k=c;c=f[n+144>>2];X=k+c|0;b=f[n+148>>2]+b|0;b=X>>>0<c>>>0?b+1|0:b;f[Y+8>>2]=X;f[Y+12>>2]=b;Y=M(j^X,b^Z,16);b=B;f[n+96>>2]=Y;f[n+100>>2]=b;b=b+U|0;U=d+Y|0;if(U>>>0<Y>>>0){b=b+1|0}f[n+88>>2]=U;f[n+92>>2]=b;f[n+48>>2]=M(h^U,b^m,63);f[n+52>>2]=B;Y=n;X=f[n+220>>2];b=i+o|0;h=x+E|0;if(h>>>0<x>>>0){b=b+1|0}U=h+f[n+216>>2]|0;b=b+X|0;b=U>>>0<h>>>0?b+1|0:b;X=b;b=b+ba|0;h=U+aa|0;if(h>>>0<U>>>0){b=b+1|0}c=h;aa=b;h=M(G^U,Q^X,32);k=x;x=B;b=z+x|0;U=h+l|0;if(U>>>0<l>>>0){b=b+1|0}d=U;X=b;U=M(k^d,b^i,24);i=B;b=i+aa|0;c=c+U|0;if(c>>>0<U>>>0){b=b+1|0}f[Y+16>>2]=c;f[Y+20>>2]=b;Y=M(h^c,b^x,16);b=B;f[n+104>>2]=Y;f[n+108>>2]=b;b=b+X|0;X=d+Y|0;if(X>>>0<Y>>>0){b=b+1|0}f[n+64>>2]=X;f[n+68>>2]=b;f[n+56>>2]=M(U^X,b^i,63);f[n+60>>2]=B;Y=n;U=R;aa=e;b=e+oa|0;X=R+fa|0;if(X>>>0<R>>>0){b=b+1|0}R=r+X|0;b=b+I|0;b=R>>>0<r>>>0?b+1|0:b;X=b;fa=M(R^w,b^t,32);d=U;h=B;b=p+h|0;U=s+fa|0;if(U>>>0<s>>>0){b=b+1|0}c=aa;aa=b;x=M(d^U,c^b,24);c=B;b=X+c|0;X=x+R|0;if(X>>>0<R>>>0){b=b+1|0}d=X;X=f[n+152>>2];R=d+X|0;b=f[n+156>>2]+b|0;b=R>>>0<X>>>0?b+1|0:b;f[Y+24>>2]=R;f[Y+28>>2]=b;R=M(R^fa,b^h,16);f[n+112>>2]=R;b=B;f[n+116>>2]=b;b=b+aa|0;aa=R+U|0;if(aa>>>0<U>>>0){b=b+1|0}U=aa;f[n+72>>2]=U;f[Y+76>>2]=b;f[n+32>>2]=M(x^U,b^c,63);f[n+36>>2]=B;b=f[n+68>>2]^(f[a+4>>2]^$);f[a>>2]=f[n+64>>2]^(f[a>>2]^y);f[a+4>>2]=b;Y=1;while(1){U=Y<<3;b=U+a|0;U=n+U|0;aa=U;R=f[U>>2]^f[b>>2];U=U- -64|0;X=f[U>>2];U=f[U+4>>2]^(f[aa+4>>2]^f[b+4>>2]);f[b>>2]=R^X;f[b+4>>2]=U;Y=Y+1|0;if((Y|0)!=8){continue}break}A=n+256|0}function Ub(a){a=a|0;var b=0,c=0,d=0,e=0,h=0,j=0,k=0,l=0,m=0,n=0,o=0;o=A-16|0;A=o;a:{b:{c:{d:{e:{f:{g:{h:{i:{j:{k:{if(a>>>0<=244){h=f[7777];j=a>>>0<11?16:a+11&-8;a=j>>>3|0;b=h>>>a|0;if(b&3){c=a+((b^-1)&1)|0;e=c<<3;b=f[e+31156>>2];a=b+8|0;d=f[b+8>>2];e=e+31148|0;l:{if((d|0)==(e|0)){f[7777]=lc(-2,c)&h;break l}f[d+12>>2]=e;f[e+8>>2]=d}c=c<<3;f[b+4>>2]=c|3;b=b+c|0;f[b+4>>2]=f[b+4>>2]|1;break a}l=f[7779];if(j>>>0<=l>>>0){break k}if(b){c=2<<a;a=(0-c|c)&b<<a;a=(0-a&a)+ -1|0;b=a>>>12&16;c=b;a=a>>>b|0;b=a>>>5&8;c=c|b;a=a>>>b|0;b=a>>>2&4;c=c|b;a=a>>>b|0;b=a>>>1&2;c=c|b;a=a>>>b|0;b=a>>>1&1;c=(c|b)+(a>>>b|0)|0;d=c<<3;b=f[d+31156>>2];a=f[b+8>>2];d=d+31148|0;m:{if((a|0)==(d|0)){h=lc(-2,c)&h;f[7777]=h;break m}f[a+12>>2]=d;f[d+8>>2]=a}a=b+8|0;f[b+4>>2]=j|3;k=b+j|0;c=c<<3;e=c-j|0;f[k+4>>2]=e|1;f[b+c>>2]=e;if(l){c=l>>>3|0;b=(c<<3)+31148|0;d=f[7782];c=1<<c;n:{if(!(c&h)){f[7777]=c|h;c=b;break n}c=f[b+8>>2]}f[b+8>>2]=d;f[c+12>>2]=d;f[d+12>>2]=b;f[d+8>>2]=c}f[7782]=k;f[7779]=e;break a}n=f[7778];if(!n){break k}a=(n&0-n)+ -1|0;b=a>>>12&16;c=b;a=a>>>b|0;b=a>>>5&8;c=c|b;a=a>>>b|0;b=a>>>2&4;c=c|b;a=a>>>b|0;b=a>>>1&2;c=c|b;a=a>>>b|0;b=a>>>1&1;b=f[((c|b)+(a>>>b|0)<<2)+31412>>2];d=(f[b+4>>2]&-8)-j|0;c=b;while(1){o:{a=f[c+16>>2];if(!a){a=f[c+20>>2];if(!a){break o}}e=(f[a+4>>2]&-8)-j|0;c=e>>>0<d>>>0;d=c?e:d;b=c?a:b;c=a;continue}break}m=f[b+24>>2];e=f[b+12>>2];if((e|0)!=(b|0)){a=f[b+8>>2];f[a+12>>2]=e;f[e+8>>2]=a;break b}c=b+20|0;a=f[c>>2];if(!a){a=f[b+16>>2];if(!a){break j}c=b+16|0}while(1){k=c;e=a;c=a+20|0;a=f[c>>2];if(a){continue}c=e+16|0;a=f[e+16>>2];if(a){continue}break}f[k>>2]=0;break b}j=-1;if(a>>>0>4294967231){break k}b=a+11|0;j=b&-8;l=f[7778];if(!l){break k}c=0-j|0;b=b>>>8|0;h=0;p:{if(!b){break p}h=31;if(j>>>0>16777215){break p}d=b+1048320>>>16&8;b=b<<d;a=b+520192>>>16&4;h=b<<a;b=h+245760>>>16&2;a=(h<<b>>>15|0)-(b|(a|d))|0;h=(a<<1|j>>>a+21&1)+28|0}d=f[(h<<2)+31412>>2];q:{r:{s:{if(!d){a=0;break s}b=j<<((h|0)==31?0:25-(h>>>1|0)|0);a=0;while(1){t:{k=(f[d+4>>2]&-8)-j|0;if(k>>>0>=c>>>0){break t}e=d;c=k;if(c){break t}c=0;a=d;break r}k=f[d+20>>2];d=f[((b>>>29&4)+d|0)+16>>2];a=k?(k|0)==(d|0)?a:k:a;b=b<<((d|0)!=0);if(d){continue}break}}if(!(a|e)){a=2<<h;a=(0-a|a)&l;if(!a){break k}a=(a&0-a)+ -1|0;b=a>>>12&16;d=b;a=a>>>b|0;b=a>>>5&8;d=d|b;a=a>>>b|0;b=a>>>2&4;d=d|b;a=a>>>b|0;b=a>>>1&2;d=d|b;a=a>>>b|0;b=a>>>1&1;a=f[((d|b)+(a>>>b|0)<<2)+31412>>2]}if(!a){break q}}while(1){d=(f[a+4>>2]&-8)-j|0;b=d>>>0<c>>>0;c=b?d:c;e=b?a:e;b=f[a+16>>2];if(b){a=b}else{a=f[a+20>>2]}if(a){continue}break}}if(!e|c>>>0>=f[7779]-j>>>0){break k}k=f[e+24>>2];b=f[e+12>>2];if((e|0)!=(b|0)){a=f[e+8>>2];f[a+12>>2]=b;f[b+8>>2]=a;break c}d=e+20|0;a=f[d>>2];if(!a){a=f[e+16>>2];if(!a){break i}d=e+16|0}while(1){h=d;b=a;d=a+20|0;a=f[d>>2];if(a){continue}d=b+16|0;a=f[b+16>>2];if(a){continue}break}f[h>>2]=0;break c}b=f[7779];if(b>>>0>=j>>>0){a=f[7782];c=b-j|0;u:{if(c>>>0>=16){f[7779]=c;d=a+j|0;f[7782]=d;f[d+4>>2]=c|1;f[a+b>>2]=c;f[a+4>>2]=j|3;break u}f[7782]=0;f[7779]=0;f[a+4>>2]=b|3;b=a+b|0;f[b+4>>2]=f[b+4>>2]|1}a=a+8|0;break a}d=f[7780];if(d>>>0>j>>>0){b=d-j|0;f[7780]=b;a=f[7783];c=a+j|0;f[7783]=c;f[c+4>>2]=b|1;f[a+4>>2]=j|3;a=a+8|0;break a}a=0;e=j+47|0;c=e;if(f[7895]){b=f[7897]}else{f[7898]=-1;f[7899]=-1;f[7896]=4096;f[7897]=4096;f[7895]=o+12&-16^1431655768;f[7900]=0;f[7888]=0;b=4096}h=c+b|0;k=0-b|0;c=h&k;if(c>>>0<=j>>>0){break a}b=f[7887];if(b){l=f[7885];m=l+c|0;if(m>>>0<=l>>>0|m>>>0>b>>>0){break a}}if(g[31552]&4){break f}v:{w:{b=f[7783];if(b){a=31556;while(1){l=f[a>>2];if(l+f[a+4>>2]>>>0>b>>>0?l>>>0<=b>>>0:0){break w}a=f[a+8>>2];if(a){continue}break}}b=_(0);if((b|0)==-1){break g}h=c;a=f[7896];d=a+ -1|0;if(d&b){h=(c-b|0)+(b+d&0-a)|0}if(h>>>0<=j>>>0|h>>>0>2147483646){break g}a=f[7887];if(a){d=f[7885];k=d+h|0;if(k>>>0<=d>>>0|k>>>0>a>>>0){break g}}a=_(h);if((b|0)!=(a|0)){break v}break e}h=k&h-d;if(h>>>0>2147483646){break g}b=_(h);if((b|0)==(f[a>>2]+f[a+4>>2]|0)){break h}a=b}if(!((a|0)==-1|j+48>>>0<=h>>>0)){b=f[7897];b=b+(e-h|0)&0-b;if(b>>>0>2147483646){b=a;break e}if((_(b)|0)!=-1){h=b+h|0;b=a;break e}_(0-h|0);break g}b=a;if((a|0)!=-1){break e}break g}e=0;break b}b=0;break c}if((b|0)!=-1){break e}}f[7888]=f[7888]|4}if(c>>>0>2147483646){break d}b=_(c);a=_(0);if(b>>>0>=a>>>0|(b|0)==-1|(a|0)==-1){break d}h=a-b|0;if(h>>>0<=j+40>>>0){break d}}a=f[7885]+h|0;f[7885]=a;if(a>>>0>i[7886]){f[7886]=a}x:{y:{z:{c=f[7783];if(c){a=31556;while(1){d=f[a>>2];e=f[a+4>>2];if((d+e|0)==(b|0)){break z}a=f[a+8>>2];if(a){continue}break}break y}a=f[7781];if(!(b>>>0>=a>>>0?a:0)){f[7781]=b}a=0;f[7890]=h;f[7889]=b;f[7785]=-1;f[7786]=f[7895];f[7892]=0;while(1){c=a<<3;d=c+31148|0;f[c+31156>>2]=d;f[c+31160>>2]=d;a=a+1|0;if((a|0)!=32){continue}break}a=h+ -40|0;c=b+8&7?-8-b&7:0;d=a-c|0;f[7780]=d;c=b+c|0;f[7783]=c;f[c+4>>2]=d|1;f[(a+b|0)+4>>2]=40;f[7784]=f[7899];break x}if(g[a+12|0]&8|b>>>0<=c>>>0|d>>>0>c>>>0){break y}f[a+4>>2]=e+h;a=c+8&7?-8-c&7:0;b=a+c|0;f[7783]=b;d=f[7780]+h|0;a=d-a|0;f[7780]=a;f[b+4>>2]=a|1;f[(c+d|0)+4>>2]=40;f[7784]=f[7899];break x}e=f[7781];if(b>>>0<e>>>0){f[7781]=b;e=0}d=b+h|0;a=31556;A:{B:{C:{D:{E:{F:{while(1){if((d|0)!=f[a>>2]){a=f[a+8>>2];if(a){continue}break F}break}if(!(g[a+12|0]&8)){break E}}a=31556;while(1){d=f[a>>2];if(d>>>0<=c>>>0){e=d+f[a+4>>2]|0;if(e>>>0>c>>>0){break D}}a=f[a+8>>2];continue}}f[a>>2]=b;f[a+4>>2]=f[a+4>>2]+h;m=(b+8&7?-8-b&7:0)+b|0;f[m+4>>2]=j|3;b=d+(d+8&7?-8-d&7:0)|0;a=(b-m|0)-j|0;k=j+m|0;if((b|0)==(c|0)){f[7783]=k;a=f[7780]+a|0;f[7780]=a;f[k+4>>2]=a|1;break B}if(f[7782]==(b|0)){f[7782]=k;a=f[7779]+a|0;f[7779]=a;f[k+4>>2]=a|1;f[a+k>>2]=a;break B}c=f[b+4>>2];if((c&3)==1){n=c&-8;G:{if(c>>>0<=255){e=c>>>3|0;c=f[b+8>>2];d=f[b+12>>2];if((d|0)==(c|0)){f[7777]=f[7777]&lc(-2,e);break G}f[c+12>>2]=d;f[d+8>>2]=c;break G}l=f[b+24>>2];h=f[b+12>>2];H:{if((h|0)!=(b|0)){c=f[b+8>>2];f[c+12>>2]=h;f[h+8>>2]=c;break H}I:{d=b+20|0;j=f[d>>2];if(j){break I}d=b+16|0;j=f[d>>2];if(j){break I}h=0;break H}while(1){c=d;h=j;d=j+20|0;j=f[d>>2];if(j){continue}d=h+16|0;j=f[h+16>>2];if(j){continue}break}f[c>>2]=0}if(!l){break G}c=f[b+28>>2];d=(c<<2)+31412|0;J:{if(f[d>>2]==(b|0)){f[d>>2]=h;if(h){break J}f[7778]=f[7778]&lc(-2,c);break G}f[l+(f[l+16>>2]==(b|0)?16:20)>>2]=h;if(!h){break G}}f[h+24>>2]=l;c=f[b+16>>2];if(c){f[h+16>>2]=c;f[c+24>>2]=h}c=f[b+20>>2];if(!c){break G}f[h+20>>2]=c;f[c+24>>2]=h}b=b+n|0;a=a+n|0}f[b+4>>2]=f[b+4>>2]&-2;f[k+4>>2]=a|1;f[a+k>>2]=a;if(a>>>0<=255){b=a>>>3|0;a=(b<<3)+31148|0;c=f[7777];b=1<<b;K:{if(!(c&b)){f[7777]=b|c;b=a;break K}b=f[a+8>>2]}f[a+8>>2]=k;f[b+12>>2]=k;f[k+12>>2]=a;f[k+8>>2]=b;break B}c=k;d=a>>>8|0;b=0;L:{if(!d){break L}b=31;if(a>>>0>16777215){break L}e=d+1048320>>>16&8;d=d<<e;b=d+520192>>>16&4;j=d<<b;d=j+245760>>>16&2;b=(j<<d>>>15|0)-(d|(b|e))|0;b=(b<<1|a>>>b+21&1)+28|0}f[c+28>>2]=b;f[k+16>>2]=0;f[k+20>>2]=0;c=(b<<2)+31412|0;d=f[7778];e=1<<b;M:{if(!(d&e)){f[7778]=d|e;f[c>>2]=k;break M}d=a<<((b|0)==31?0:25-(b>>>1|0)|0);b=f[c>>2];while(1){c=b;if((f[b+4>>2]&-8)==(a|0)){break C}b=d>>>29|0;d=d<<1;e=(b&4)+c|0;b=f[e+16>>2];if(b){continue}break}f[e+16>>2]=k}f[k+24>>2]=c;f[k+12>>2]=k;f[k+8>>2]=k;break B}a=h+ -40|0;d=b+8&7?-8-b&7:0;k=a-d|0;f[7780]=k;d=b+d|0;f[7783]=d;f[d+4>>2]=k|1;f[(a+b|0)+4>>2]=40;f[7784]=f[7899];a=(e+(e+ -39&7?39-e&7:0)|0)+ -47|0;d=a>>>0<c+16>>>0?c:a;f[d+4>>2]=27;a=f[7892];f[d+16>>2]=f[7891];f[d+20>>2]=a;a=f[7890];f[d+8>>2]=f[7889];f[d+12>>2]=a;f[7891]=d+8;f[7890]=h;f[7889]=b;f[7892]=0;a=d+24|0;while(1){f[a+4>>2]=7;b=a+8|0;a=a+4|0;if(e>>>0>b>>>0){continue}break}if((c|0)==(d|0)){break x}f[d+4>>2]=f[d+4>>2]&-2;e=d-c|0;f[c+4>>2]=e|1;f[d>>2]=e;if(e>>>0<=255){b=e>>>3|0;a=(b<<3)+31148|0;d=f[7777];b=1<<b;N:{if(!(d&b)){f[7777]=b|d;b=a;break N}b=f[a+8>>2]}f[a+8>>2]=c;f[b+12>>2]=c;f[c+12>>2]=a;f[c+8>>2]=b;break x}f[c+16>>2]=0;f[c+20>>2]=0;b=c;d=e>>>8|0;a=0;O:{if(!d){break O}a=31;if(e>>>0>16777215){break O}h=d+1048320>>>16&8;d=d<<h;a=d+520192>>>16&4;k=d<<a;d=k+245760>>>16&2;a=(k<<d>>>15|0)-(d|(a|h))|0;a=(a<<1|e>>>a+21&1)+28|0}f[b+28>>2]=a;b=(a<<2)+31412|0;d=f[7778];h=1<<a;P:{if(!(d&h)){f[7778]=d|h;f[b>>2]=c;f[c+24>>2]=b;break P}a=e<<((a|0)==31?0:25-(a>>>1|0)|0);b=f[b>>2];while(1){d=b;if((e|0)==(f[b+4>>2]&-8)){break A}b=a>>>29|0;a=a<<1;h=d+(b&4)|0;b=f[h+16>>2];if(b){continue}break}f[h+16>>2]=c;f[c+24>>2]=d}f[c+12>>2]=c;f[c+8>>2]=c;break x}a=f[c+8>>2];f[a+12>>2]=k;f[c+8>>2]=k;f[k+24>>2]=0;f[k+12>>2]=c;f[k+8>>2]=a}a=m+8|0;break a}a=f[d+8>>2];f[a+12>>2]=c;f[d+8>>2]=c;f[c+24>>2]=0;f[c+12>>2]=d;f[c+8>>2]=a}a=f[7780];if(a>>>0<=j>>>0){break d}b=a-j|0;f[7780]=b;a=f[7783];c=a+j|0;f[7783]=c;f[c+4>>2]=b|1;f[a+4>>2]=j|3;a=a+8|0;break a}f[7776]=48;a=0;break a}Q:{if(!k){break Q}a=f[e+28>>2];d=(a<<2)+31412|0;R:{if(f[d>>2]==(e|0)){f[d>>2]=b;if(b){break R}l=lc(-2,a)&l;f[7778]=l;break Q}f[k+(f[k+16>>2]==(e|0)?16:20)>>2]=b;if(!b){break Q}}f[b+24>>2]=k;a=f[e+16>>2];if(a){f[b+16>>2]=a;f[a+24>>2]=b}a=f[e+20>>2];if(!a){break Q}f[b+20>>2]=a;f[a+24>>2]=b}S:{if(c>>>0<=15){a=c+j|0;f[e+4>>2]=a|3;a=a+e|0;f[a+4>>2]=f[a+4>>2]|1;break S}f[e+4>>2]=j|3;d=e+j|0;f[d+4>>2]=c|1;f[c+d>>2]=c;if(c>>>0<=255){b=c>>>3|0;a=(b<<3)+31148|0;c=f[7777];b=1<<b;T:{if(!(c&b)){f[7777]=b|c;b=a;break T}b=f[a+8>>2]}f[a+8>>2]=d;f[b+12>>2]=d;f[d+12>>2]=a;f[d+8>>2]=b;break S}b=d;j=c>>>8|0;a=0;U:{if(!j){break U}a=31;if(c>>>0>16777215){break U}h=j+1048320>>>16&8;j=j<<h;a=j+520192>>>16&4;k=j<<a;j=k+245760>>>16&2;a=(k<<j>>>15|0)-(j|(a|h))|0;a=(a<<1|c>>>a+21&1)+28|0}f[b+28>>2]=a;f[d+16>>2]=0;f[d+20>>2]=0;b=(a<<2)+31412|0;V:{j=1<<a;W:{if(!(j&l)){f[7778]=j|l;f[b>>2]=d;break W}a=c<<((a|0)==31?0:25-(a>>>1|0)|0);j=f[b>>2];while(1){b=j;if((f[b+4>>2]&-8)==(c|0)){break V}j=a>>>29|0;a=a<<1;h=(j&4)+b|0;j=f[h+16>>2];if(j){continue}break}f[h+16>>2]=d}f[d+24>>2]=b;f[d+12>>2]=d;f[d+8>>2]=d;break S}a=f[b+8>>2];f[a+12>>2]=d;f[b+8>>2]=d;f[d+24>>2]=0;f[d+12>>2]=b;f[d+8>>2]=a}a=e+8|0;break a}X:{if(!m){break X}a=f[b+28>>2];c=(a<<2)+31412|0;Y:{if(f[c>>2]==(b|0)){f[c>>2]=e;if(e){break Y}f[7778]=lc(-2,a)&n;break X}f[m+(f[m+16>>2]==(b|0)?16:20)>>2]=e;if(!e){break X}}f[e+24>>2]=m;a=f[b+16>>2];if(a){f[e+16>>2]=a;f[a+24>>2]=e}a=f[b+20>>2];if(!a){break X}f[e+20>>2]=a;f[a+24>>2]=e}Z:{if(d>>>0<=15){a=d+j|0;f[b+4>>2]=a|3;a=a+b|0;f[a+4>>2]=f[a+4>>2]|1;break Z}f[b+4>>2]=j|3;j=b+j|0;f[j+4>>2]=d|1;f[d+j>>2]=d;if(l){c=l>>>3|0;a=(c<<3)+31148|0;e=f[7782];c=1<<c;_:{if(!(c&h)){f[7777]=c|h;c=a;break _}c=f[a+8>>2]}f[a+8>>2]=e;f[c+12>>2]=e;f[e+12>>2]=a;f[e+8>>2]=c}f[7782]=j;f[7779]=d}a=b+8|0}A=o+16|0;return a|0}function Ma(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0;n=a;p=f[c>>2];s=p;u=f[b+8>>2];g=kc(u,0,1073741823);h=B;o=h;D=f[b+12>>2];r=kc(D,0,1073741823);w=B;z=f[b+16>>2];i=kc(z,0,1073741823);v=B;d=v+w|0;e=i;j=e+r|0;if(j>>>0<e>>>0){d=d+1|0}A=f[b+28>>2];e=kc(A,0,103979646);j=e+j|0;d=B+d|0;d=j>>>0<e>>>0?d+1|0:d;E=f[b+24>>2];e=kc(E,0,25712450);j=e+j|0;d=B+d|0;d=j>>>0<e>>>0?d+1|0:d;x=f[b+20>>2];e=kc(x,0,1073736481);j=e+j|0;d=B+d|0;d=j>>>0<e>>>0?d+1|0:d;e=j;j=e+g|0;d=d+h|0;d=j>>>0<e>>>0?d+1|0:d;k=f[b+4>>2];e=kc(k,0,1048575);h=e+j|0;d=B+d|0;d=h>>>0<e>>>0?d+1|0:d;e=h;C=f[b+32>>2];h=kc(C,0,913544844);j=e+h|0;e=B+d|0;m=j;j=j>>>0<h>>>0?e+1|0:e;q=kc(k,0,1073741823);h=B;l=h;d=o+w|0;e=g;k=e+r|0;if(k>>>0<e>>>0){d=d+1|0}e=kc(A,0,913544844);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;e=kc(E,0,103979646);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;e=kc(x,0,25712450);k=e+k|0;d=B+d|0;d=k>>>0<e>>>0?d+1|0:d;e=k;k=kc(z,0,1073736481);t=e+k|0;e=B+d|0;e=t>>>0<k>>>0?e+1|0:e;k=t;t=k+q|0;d=e+h|0;h=f[b>>2];b=kc(h,0,1048575);e=b+t|0;d=B+(t>>>0<k>>>0?d+1|0:d)|0;d=e>>>0<b>>>0?d+1|0:d;b=kc(C,0,170660635);e=b+e|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=d;t=e;d=l+o|0;e=g+q|0;if(e>>>0<g>>>0){d=d+1|0}g=kc(A,0,170660635);k=g+e|0;e=B+d|0;e=k>>>0<g>>>0?e+1|0:e;g=kc(E,0,913544844);k=g+k|0;d=B+e|0;d=k>>>0<g>>>0?d+1|0:d;e=kc(x,0,103979646);g=e+k|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=kc(z,0,25712450);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=kc(D,0,1073736481);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=kc(h,0,1073741823);h=e+g|0;e=B+d|0;e=h>>>0<g>>>0?e+1|0:e;g=h;d=e>>>30|0;e=(e&1073741823)<<2|g>>>30;g=t+e|0;d=b+d|0;h=g;d=g>>>0<e>>>0?d+1|0:d;g=d;b=(d&1073741823)<<2|h>>>30;k=b+m|0;d=(d>>>30|0)+j|0;d=k>>>0<b>>>0?d+1|0:d;b=d;j=0;t=j;e=k;F=e<<6&1073741760|((g&16777215)<<8|h>>>24)&63;g=kc(F,j,485872621);d=B;e=g&1073741823;o=S(s,e);f[n>>2]=(p+(o<<30)|0)-e;j=a;q=f[c+4>>2];p=q;e=d;d=d>>>30|0;e=(e&1073741823)<<2|g>>>30;g=kc(F,t,541690985)+e|0;d=d+B|0;s=g;h=g>>>0<e>>>0?d+1|0:d;l=kc(x,0,1073741823);y=B;d=y+v|0;e=l;g=e+i|0;if(g>>>0<e>>>0){d=d+1|0}e=kc(A,0,25712450);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=kc(E,0,1073736481);n=e+g|0;e=B+d|0;e=n>>>0<g>>>0?e+1|0:e;g=r;n=g+n|0;d=e+w|0;d=n>>>0<g>>>0?d+1|0:d;e=kc(u,0,1048575);g=e+n|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=kc(C,0,103979646);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=d;m=g;g=b;d=g>>>30|0;g=(g&1073741823)<<2|k>>>30;n=m+g|0;d=d+e|0;m=n;g=n>>>0<g>>>0?d+1|0:d;n=0;L=n;e=m;G=e<<6&1073741760|((b&16777215)<<8|k>>>24)&63;b=kc(G,n,485872621);d=b+s|0;e=B+h|0;u=d;h=d>>>0<b>>>0?e+1|0:e;b=o+(d&1073741823)|0;o=S(p,b);f[j+4>>2]=(q+(o<<30)|0)-b;k=a;q=f[c+8>>2];p=q;b=kc(G,n,541690985);d=B;e=b;b=kc(F,t,796511589);e=e+b|0;d=B+d|0;s=e;n=e>>>0<b>>>0?d+1|0:d;j=kc(E,0,1073741823);w=B;d=w+y|0;b=j;e=b+l|0;if(e>>>0<b>>>0){d=d+1|0}y=e;b=kc(A,0,1073736481);e=e+b|0;l=d;d=d+B|0;d=e>>>0<b>>>0?d+1|0:d;b=e+i|0;d=d+v|0;d=b>>>0<i>>>0?d+1|0:d;e=b;b=kc(D,0,1048575);i=e+b|0;e=B+d|0;e=i>>>0<b>>>0?e+1|0:e;b=kc(C,0,25712450);i=b+i|0;d=B+e|0;e=i;b=e>>>0<b>>>0?d+1|0:d;e=g;d=e>>>30|0;e=(e&1073741823)<<2|m>>>30;i=i+e|0;d=b+d|0;r=i;b=i>>>0<e>>>0?d+1|0:d;i=0;M=i;e=r;H=e<<6&1073741760|((g&16777215)<<8|m>>>24)&63;e=kc(H,i,485872621);g=e+s|0;d=B+n|0;d=g>>>0<e>>>0?d+1|0:d;e=d;m=g;d=h>>>30|0;g=(h&1073741823)<<2|u>>>30;h=m+g|0;e=d+e|0;v=h;h=h>>>0<g>>>0?e+1|0:e;d=o+(v&1073741823)|0;o=S(p,d);f[k+8>>2]=(q+(o<<30)|0)-d;q=f[c+12>>2];p=q;e=kc(G,L,796511589);d=B;g=e;e=kc(F,t,935229352);g=g+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=kc(H,i,541690985);g=e+g|0;d=B+d|0;s=g;n=g>>>0<e>>>0?d+1|0:d;i=kc(A,0,1073741823);u=B;d=u+l|0;e=i;g=e+y|0;if(g>>>0<e>>>0){d=d+1|0}e=kc(z,0,1048575);g=e+g|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;e=g;g=kc(C,0,1073736481);l=e+g|0;e=B+d|0;e=l>>>0<g>>>0?e+1|0:e;g=b;d=g>>>30|0;g=(g&1073741823)<<2|r>>>30;l=g+l|0;d=d+e|0;m=l;g=l>>>0<g>>>0?d+1|0:d;l=0;J=l;e=m;I=e<<6&1073741760|((b&16777215)<<8|r>>>24)&63;b=kc(I,l,485872621);e=b+s|0;d=B+n|0;d=e>>>0<b>>>0?d+1|0:d;l=e;e=h>>>30|0;b=(h&1073741823)<<2|v>>>30;h=l+b|0;d=d+e|0;z=h;h=h>>>0<b>>>0?d+1|0:d;b=o+(z&1073741823)|0;y=S(p,b);f[k+12>>2]=(q+(y<<30)|0)-b;l=a;o=f[c+16>>2];p=o;b=kc(G,L,935229352);d=B;e=b;b=kc(F,t,20);e=e+b|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=kc(H,M,796511589);e=b+e|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=kc(I,J,541690985);e=b+e|0;d=B+d|0;s=e;k=e>>>0<b>>>0?d+1|0:d;D=kc(C,0,1073741823);q=B;e=u+q|0;b=i+D|0;if(b>>>0<i>>>0){e=e+1|0}v=b;n=e;b=kc(x,0,1048575)+j|0;d=w+B|0;d=b>>>0<j>>>0?d+1|0:d;i=b+v|0;d=d+e|0;d=i>>>0<b>>>0?d+1|0:d;e=g>>>30|0;b=(g&1073741823)<<2|m>>>30;i=b+i|0;d=d+e|0;r=i;b=i>>>0<b>>>0?d+1|0:d;i=0;K=i;e=r;x=e<<6&1073741760|((g&16777215)<<8|m>>>24)&63;e=kc(x,i,485872621);g=e+s|0;d=B+k|0;d=g>>>0<e>>>0?d+1|0:d;e=d;i=g;d=h>>>30|0;g=(h&1073741823)<<2|z>>>30;h=i+g|0;d=d+e|0;u=h;h=h>>>0<g>>>0?d+1|0:d;d=y+(u&1073741823)|0;p=S(p,d);f[l+16>>2]=(o+(p<<30)|0)-d;j=a;s=f[c+20>>2];m=s;e=kc(H,M,935229352);d=B;a=e;e=kc(G,L,20);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=kc(I,J,796511589);i=a+g|0;e=B+d|0;e=i>>>0<g>>>0?e+1|0:e;g=kc(x,K,541690985);i=g+i|0;d=B+e|0;l=i;i=i>>>0<g>>>0?d+1|0:d;e=kc(E,0,1048575);g=e+v|0;d=B+n|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=b;e=g>>>30|0;g=(g&1073741823)<<2|r>>>30;n=a+g|0;d=d+e|0;d=n>>>0<g>>>0?d+1|0:d;g=d;k=0;v=k;e=n;w=e<<6&1073741760|((b&16777215)<<8|r>>>24)&63;b=kc(w,k,485872621);e=b+l|0;d=B+i|0;d=e>>>0<b>>>0?d+1|0:d;b=d;a=e;d=h>>>30|0;e=(h&1073741823)<<2|u>>>30;h=a+e|0;d=b+d|0;o=h;h=h>>>0<e>>>0?d+1|0:d;b=p+(o&1073741823)|0;p=S(m,b);f[j+20>>2]=(m+(p<<30)|0)-b;s=f[c+24>>2];m=s;b=kc(I,J,935229352);d=B;a=b;b=kc(H,M,20);e=a+b|0;d=B+d|0;d=e>>>0<b>>>0?d+1|0:d;b=kc(x,K,796511589);i=b+e|0;e=B+d|0;e=i>>>0<b>>>0?e+1|0:e;b=kc(w,k,541690985);i=b+i|0;d=B+e|0;r=i;i=i>>>0<b>>>0?d+1|0:d;b=kc(A,0,1048575);e=b+D|0;d=B+q|0;d=e>>>0<b>>>0?d+1|0:d;a=e;e=g>>>30|0;b=(g&1073741823)<<2|n>>>30;k=a+b|0;d=d+e|0;d=k>>>0<b>>>0?d+1|0:d;b=d;l=0;u=l;e=k;y=e<<6&1073741760|((g&16777215)<<8|n>>>24)&63;e=kc(y,l,485872621);g=e+r|0;d=B+i|0;d=g>>>0<e>>>0?d+1|0:d;e=d;a=g;d=h>>>30|0;g=(h&1073741823)<<2|o>>>30;h=a+g|0;d=d+e|0;o=h;h=h>>>0<g>>>0?d+1|0:d;d=p+(o&1073741823)|0;q=S(m,d);f[j+24>>2]=(m+(q<<30)|0)-d;i=j;p=f[c+28>>2];e=kc(x,K,935229352);d=B;a=e;e=kc(I,J,20);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;a=g;g=kc(w,v,796511589);j=a+g|0;e=B+d|0;e=j>>>0<g>>>0?e+1|0:e;g=kc(y,l,541690985);j=g+j|0;d=B+e|0;l=j;e=j>>>0<g>>>0?d+1|0:d;g=b;d=g>>>30|0;g=(g&1073741823)<<2|k>>>30;j=kc(C,0,1048575)+g|0;d=d+B|0;n=j;g=j>>>0<g>>>0?d+1|0:d;m=0;r=m;a=l;l=j<<6&1073741760|((b&16777215)<<8|k>>>24)&63;b=kc(l,m,485872621);j=a+b|0;d=B+e|0;e=j;b=e>>>0<b>>>0?d+1|0:d;d=h>>>30|0;h=(h&1073741823)<<2|o>>>30;j=h+e|0;e=b+d|0;o=j;b=j>>>0<h>>>0?e+1|0:e;d=q+(j&1073741823)|0;k=S(p,d);f[i+28>>2]=(p+(k<<30)|0)-d;h=i;j=f[c+32>>2];i=j;m=F;d=t<<12|m>>>20;c=m<<12;m=kc(x,K,20)+c|0;d=d+B|0;d=m>>>0<c>>>0?d+1|0:d;c=kc(w,v,935229352);e=c+m|0;d=B+d|0;d=e>>>0<c>>>0?d+1|0:d;a=e;e=kc((g&16777215)<<8|n>>>24,g>>>24|0,485872621);g=a+e|0;d=B+d|0;d=g>>>0<e>>>0?d+1|0:d;c=kc(y,u,796511589);g=c+g|0;e=B+d|0;e=g>>>0<c>>>0?e+1|0:e;c=kc(l,r,541690985);g=c+g|0;e=g;e=((b&1073741823)<<2|o>>>30)+e|0;b=k+(e&16777215)|0;f[h+32>>2]=(i+(S(i,b)<<24)|0)-b;sa(h);sa(h)}function N(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,P=0,Q=0,R=0,S=0;t=f[b+4>>2];h=f[c+4>>2];O=h<<1;d=kc(t,0,O);j=B;u=f[b>>2];g=f[c+8>>2];H=g;o=kc(u,0,g);e=o+d|0;d=B+j|0;d=e>>>0<o>>>0?d+1|0:d;v=f[b+8>>2];w=f[c>>2];j=kc(v,0,w);e=j+e|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;x=f[b+12>>2];Q=f[c+36>>2];M=l(Q,38);j=kc(x,0,M);e=j+e|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;y=f[b+16>>2];K=f[c+32>>2];s=l(K,19);o=kc(y,0,s);j=o+e|0;e=B+d|0;e=j>>>0<o>>>0?e+1|0:e;z=f[b+20>>2];p=f[c+28>>2];L=p&2147483647;n=l(L,38);o=kc(z,0,n);j=o+j|0;d=B+e|0;d=j>>>0<o>>>0?d+1|0:d;e=j;A=f[b+24>>2];m=f[c+24>>2];G=l(m,19);j=kc(A,0,G);e=e+j|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;C=f[b+28>>2];o=f[c+20>>2];q=o&2147483647;R=l(q,38);i=kc(C,0,R);j=i+e|0;e=B+d|0;e=j>>>0<i>>>0?e+1|0:e;D=f[b+32>>2];P=f[c+16>>2];r=l(P,19);i=kc(D,0,r);j=i+j|0;d=B+e|0;d=j>>>0<i>>>0?d+1|0:d;E=f[b+36>>2];i=f[c+12>>2];F=i&2147483647;I=l(F,38);c=kc(E,0,I);b=c+j|0;d=B+d|0;k=b;c=b>>>0<c>>>0?d+1|0:d;b=kc(t,0,w);d=B;j=h;J=kc(u,0,j);b=J+b|0;e=B+d|0;e=b>>>0<J>>>0?e+1|0:e;J=l(Q,19);h=kc(v,0,J);b=h+b|0;d=B+e|0;d=b>>>0<h>>>0?d+1|0:d;e=kc(s,0,x);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;L=l(L,19);h=kc(y,0,L);e=h+b|0;b=B+d|0;b=e>>>0<h>>>0?b+1|0:b;h=kc(z,0,G);e=h+e|0;d=B+b|0;d=e>>>0<h>>>0?d+1|0:d;q=l(q,19);h=kc(A,0,q);b=h+e|0;e=B+d|0;e=b>>>0<h>>>0?e+1|0:e;h=kc(C,0,r);b=h+b|0;d=B+e|0;d=b>>>0<h>>>0?d+1|0:d;e=kc(D,0,l(F,19));b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;F=l(g,19);g=kc(E,0,F);e=g+b|0;b=B+d|0;d=e;g=d>>>0<g>>>0?b+1|0:b;b=kc(t,0,M);e=B;N=k;S=d;k=kc(u,0,w);b=k+b|0;d=B+e|0;d=b>>>0<k>>>0?d+1|0:d;k=kc(s,0,v);b=k+b|0;e=B+d|0;e=b>>>0<k>>>0?e+1|0:e;k=kc(x,0,n);b=k+b|0;d=B+e|0;d=b>>>0<k>>>0?d+1|0:d;e=kc(y,0,G);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=kc(z,0,R);e=k+b|0;b=B+d|0;b=e>>>0<k>>>0?b+1|0:b;k=kc(A,0,r);e=k+e|0;d=B+b|0;I=kc(C,0,I);b=I+e|0;e=B+(e>>>0<k>>>0?d+1|0:d)|0;e=b>>>0<I>>>0?e+1|0:e;F=kc(D,0,F);b=F+b|0;d=B+e|0;d=b>>>0<F>>>0?d+1|0:d;e=kc(E,0,l(j,38));b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;F=b;e=d>>>26|0;h=(d&67108863)<<6|b>>>26;d=S+h|0;b=e+g|0;I=d;e=d;b=d>>>0<h>>>0?b+1|0:b;d=b>>>25|0;e=(b&33554431)<<7|e>>>25;b=N+e|0;d=c+d|0;g=b;c=b>>>0<e>>>0?d+1|0:d;f[a+8>>2]=b&67108863;h=a;b=kc(t,0,H);d=B;N=i;e=kc(u,0,i);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=kc(v,0,j);b=k+b|0;e=B+d|0;e=b>>>0<k>>>0?e+1|0:e;k=kc(w,0,x);d=k+b|0;b=B+e|0;b=d>>>0<k>>>0?b+1|0:b;k=kc(y,0,J);e=k+d|0;d=B+b|0;d=e>>>0<k>>>0?d+1|0:d;b=e;e=kc(s,0,z);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(A,0,L);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;k=kc(G,0,C);b=k+b|0;e=B+d|0;q=kc(D,0,q);d=q+b|0;b=B+(b>>>0<k>>>0?e+1|0:e)|0;r=kc(r,0,E);e=r+d|0;d=B+(d>>>0<q>>>0?b+1|0:b)|0;d=e>>>0<r>>>0?d+1|0:d;b=e;e=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=b+c|0;d=d+e|0;r=b;c=b>>>0<c>>>0?d+1|0:d;f[h+12>>2]=b&33554431;g=h;q=i<<1;b=kc(t,0,q);d=B;h=P;e=kc(u,0,h);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;i=kc(v,0,H);e=i+b|0;b=B+d|0;b=e>>>0<i>>>0?b+1|0:b;i=kc(x,0,O);e=i+e|0;d=B+b|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(w,0,y);b=i+e|0;e=B+d|0;e=b>>>0<i>>>0?e+1|0:e;i=kc(z,0,M);b=i+b|0;d=B+e|0;d=b>>>0<i>>>0?d+1|0:d;e=kc(s,0,A);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;i=kc(C,0,n);e=i+b|0;b=B+d|0;b=e>>>0<i>>>0?b+1|0:b;i=kc(G,0,D);e=i+e|0;d=B+b|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(E,0,R);b=i+e|0;e=B+d|0;e=b>>>0<i>>>0?e+1|0:e;d=b;b=c>>>25|0;i=(c&33554431)<<7|r>>>25;c=d+i|0;d=b+e|0;r=c;c=c>>>0<i>>>0?d+1|0:d;f[g+16>>2]=r&67108863;b=kc(t,0,h);d=B;i=o;e=kc(u,0,i);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(v,0,N);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;g=kc(x,0,H);b=g+b|0;e=B+d|0;e=b>>>0<g>>>0?e+1|0:e;g=kc(y,0,j);d=g+b|0;b=B+e|0;b=d>>>0<g>>>0?b+1|0:b;g=kc(w,0,z);e=g+d|0;d=B+b|0;d=e>>>0<g>>>0?d+1|0:d;b=e;e=kc(A,0,J);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(s,0,C);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;g=kc(D,0,L);b=g+b|0;e=B+d|0;e=b>>>0<g>>>0?e+1|0:e;g=kc(G,0,E);d=g+b|0;b=B+e|0;b=d>>>0<g>>>0?b+1|0:b;g=d;d=c>>>26|0;e=(c&67108863)<<6|r>>>26;c=g+e|0;d=b+d|0;g=c;c=c>>>0<e>>>0?d+1|0:d;f[a+20>>2]=g&33554431;G=i<<1;b=kc(t,0,G);d=B;o=m;e=kc(u,0,m);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;m=kc(v,0,h);b=m+b|0;e=B+d|0;e=b>>>0<m>>>0?e+1|0:e;m=kc(x,0,q);d=m+b|0;b=B+e|0;b=d>>>0<m>>>0?b+1|0:b;m=kc(y,0,H);e=m+d|0;d=B+b|0;d=e>>>0<m>>>0?d+1|0:d;b=e;e=kc(z,0,O);b=b+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(w,0,A);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;m=kc(C,0,M);b=m+b|0;e=B+d|0;e=b>>>0<m>>>0?e+1|0:e;m=kc(s,0,D);d=m+b|0;b=B+e|0;b=d>>>0<m>>>0?b+1|0:b;m=kc(E,0,n);e=m+d|0;d=B+b|0;d=e>>>0<m>>>0?d+1|0:d;b=e;e=c>>>25|0;c=(c&33554431)<<7|g>>>25;b=b+c|0;d=d+e|0;g=b;c=b>>>0<c>>>0?d+1|0:d;f[a+24>>2]=b&67108863;m=a;b=kc(t,0,o);d=B;P=p;e=kc(u,0,p);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;n=kc(v,0,i);e=n+b|0;b=B+d|0;b=e>>>0<n>>>0?b+1|0:b;n=kc(x,0,h);e=n+e|0;d=B+b|0;d=e>>>0<n>>>0?d+1|0:d;n=kc(y,0,N);b=n+e|0;e=B+d|0;e=b>>>0<n>>>0?e+1|0:e;n=kc(z,0,H);b=n+b|0;d=B+e|0;d=b>>>0<n>>>0?d+1|0:d;e=kc(A,0,j);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;n=kc(w,0,C);e=n+b|0;b=B+d|0;b=e>>>0<n>>>0?b+1|0:b;n=kc(D,0,J);e=n+e|0;d=B+b|0;s=kc(s,0,E);b=s+e|0;e=B+(e>>>0<n>>>0?d+1|0:d)|0;e=b>>>0<s>>>0?e+1|0:e;a=b;b=c>>>26|0;g=(c&67108863)<<6|g>>>26;c=a+g|0;d=b+e|0;d=c>>>0<g>>>0?d+1|0:d;g=c;c=d;f[m+28>>2]=g&33554431;b=kc(t,0,p<<1);d=B;e=kc(u,0,K);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(v,0,o);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;p=kc(x,0,G);b=p+b|0;e=B+d|0;e=b>>>0<p>>>0?e+1|0:e;p=kc(y,0,h);d=p+b|0;b=B+e|0;b=d>>>0<p>>>0?b+1|0:b;p=kc(z,0,q);e=p+d|0;d=B+b|0;d=e>>>0<p>>>0?d+1|0:d;a=e;e=kc(A,0,H);b=a+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(C,0,O);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;p=kc(w,0,D);b=p+b|0;e=B+d|0;e=b>>>0<p>>>0?e+1|0:e;p=kc(E,0,M);d=p+b|0;b=B+e|0;b=d>>>0<p>>>0?b+1|0:b;a=d;d=c>>>25|0;e=(c&33554431)<<7|g>>>25;c=a+e|0;d=b+d|0;g=c;c=c>>>0<e>>>0?d+1|0:d;f[m+32>>2]=g&67108863;b=kc(t,0,K);d=B;e=kc(u,0,Q);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;K=kc(v,0,P);b=K+b|0;e=B+d|0;o=kc(x,0,o);d=o+b|0;b=B+(b>>>0<K>>>0?e+1|0:e)|0;b=d>>>0<o>>>0?b+1|0:b;o=kc(y,0,i);e=o+d|0;d=B+b|0;d=e>>>0<o>>>0?d+1|0:d;a=e;e=kc(z,0,h);b=a+e|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(A,0,N);b=e+b|0;d=B+d|0;d=b>>>0<e>>>0?d+1|0:d;h=kc(C,0,H);b=h+b|0;e=B+d|0;e=b>>>0<h>>>0?e+1|0:e;h=kc(D,0,j);d=h+b|0;b=B+e|0;b=d>>>0<h>>>0?b+1|0:b;h=kc(w,0,E);e=h+d|0;d=B+b|0;d=e>>>0<h>>>0?d+1|0:d;a=e;e=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=a+c|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;f[m+36>>2]=b&33554431;c=F&67108863;b=kc((d&33554431)<<7|b>>>25,0,19)+c|0;d=B;d=b>>>0<c>>>0?d+1|0:d;f[m>>2]=b&67108863;f[m+4>>2]=(I&33554431)+((d&67108863)<<6|b>>>26)}function la(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,N=0,O=0,P=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0;z=A-640|0;A=z;Ya(z,b);b=16;j=f[z>>2];g=f[z+4>>2];while(1){l=(b<<3)+z|0;e=l;h=j;k=g;c=e+ -16|0;j=f[c+4>>2];g=f[c>>2];c=j>>>6|0;d=j;j=M(g,d,19)^((d&63)<<26|g>>>6);c=B^c;i=M(g,d,61)^j;g=e+ -56|0;d=f[g>>2];j=i+d|0;c=f[g+4>>2]+(B^c)|0;c=j>>>0<d>>>0?c+1|0:c;g=j;j=g+h|0;c=c+k|0;p=j;h=j>>>0<g>>>0?c+1|0:c;c=e+ -120|0;d=f[c+4>>2];j=f[c>>2];c=d>>>7|0;g=d;d=M(j,d,1)^((d&127)<<25|j>>>7);c=B^c;k=M(j,g,8)^d;d=k+p|0;c=(B^c)+h|0;f[e>>2]=d;f[e+4>>2]=d>>>0<k>>>0?c+1|0:c;b=b+1|0;if((b|0)!=80){continue}break}b=0;c=a;g=f[c+148>>2];P=f[c+144>>2];j=P;X=g;d=f[c+156>>2];Y=d;Q=f[c+152>>2];m=Q;l=f[c+204>>2];Z=l;R=f[c+200>>2];r=R;q=f[c+196>>2];_=q;S=f[c+192>>2];s=S;p=f[c+188>>2];$=p;T=f[c+184>>2];u=T;k=f[c+180>>2];aa=k;U=f[c+176>>2];i=U;e=f[c+172>>2];ba=e;V=f[c+168>>2];n=V;h=f[c+164>>2];ca=h;W=f[c+160>>2];o=W;while(1){v=M(i,k,14);x=B;D=M(i,k,18);E=B;F=M(i,k,41);G=B;H=M(j,g,28);I=B;J=M(j,g,34);K=B;L=M(j,g,39);C=B;t=e;y=b<<3;c=y+29792|0;e=f[c>>2];w=f[c+4>>2];O=e;c=l+(q^k&(p^q))|0;e=r+(s^i&(s^u))|0;if(e>>>0<r>>>0){c=c+1|0}l=v^D^F;e=l+e|0;c=(x^E^G)+c|0;c=e>>>0<l>>>0?c+1|0:c;l=e;e=O+e|0;c=c+w|0;c=e>>>0<l>>>0?c+1|0:c;l=y+z|0;r=f[l>>2];e=r+e|0;c=f[l+4>>2]+c|0;c=e>>>0<r>>>0?c+1|0:c;l=c;c=c+t|0;r=e+n|0;if(r>>>0<e>>>0){c=c+1|0}n=r;v=M(n,c,14);x=B;r=c;D=M(n,c,18);E=B;F=M(n,c,41);G=B;c=(h&(d|g)|d&g)+l|0;l=e;e=o&(j|m)|j&m;l=l+e|0;if(l>>>0<e>>>0){c=c+1|0}e=l;l=H^J^L;e=e+l|0;c=(I^K^C)+c|0;c=e>>>0<l>>>0?c+1|0:c;l=e;H=M(e,c,28);I=B;e=c;J=M(l,c,34);K=B;L=M(l,c,39);C=B;t=h;w=y|8;c=w+29792|0;h=f[c>>2];N=f[c+4>>2];O=h;c=q+(p^(k^p)&r)|0;h=s+(u^(i^u)&n)|0;if(h>>>0<s>>>0){c=c+1|0}q=v^D^F;h=q+h|0;c=(x^E^G)+c|0;c=h>>>0<q>>>0?c+1|0:c;q=h;h=O+h|0;c=c+N|0;c=h>>>0<q>>>0?c+1|0:c;q=z+w|0;s=f[q>>2];h=s+h|0;c=f[q+4>>2]+c|0;c=h>>>0<s>>>0?c+1|0:c;q=c;c=c+t|0;s=h+o|0;if(s>>>0<h>>>0){c=c+1|0}o=s;v=M(o,c,14);x=B;s=c;D=M(o,c,18);E=B;F=M(o,c,41);G=B;c=(d&(e|g)|e&g)+q|0;t=h;h=m&(j|l)|j&l;q=t+h|0;if(q>>>0<h>>>0){c=c+1|0}h=q;q=H^J^L;h=h+q|0;c=(I^K^C)+c|0;c=h>>>0<q>>>0?c+1|0:c;q=h;H=M(h,c,28);I=B;h=c;J=M(q,c,34);K=B;L=M(q,c,39);C=B;t=d;w=y|16;c=w+29792|0;d=f[c>>2];N=f[c+4>>2];O=d;c=p+(k^(k^r)&s)|0;d=u+(i^(i^n)&o)|0;if(d>>>0<u>>>0){c=c+1|0}p=v^D^F;d=p+d|0;c=(x^E^G)+c|0;c=d>>>0<p>>>0?c+1|0:c;p=d;d=O+d|0;c=c+N|0;c=d>>>0<p>>>0?c+1|0:c;p=z+w|0;u=f[p>>2];d=u+d|0;c=f[p+4>>2]+c|0;c=d>>>0<u>>>0?c+1|0:c;p=t;t=c;c=p+c|0;m=d+m|0;if(m>>>0<d>>>0){c=c+1|0}p=m;v=M(m,c,14);x=B;u=c;D=M(m,c,18);E=B;F=M(m,c,41);G=B;c=(g&(e|h)|e&h)+t|0;t=d;d=j&(l|q)|l&q;m=t+d|0;if(m>>>0<d>>>0){c=c+1|0}d=m;m=H^J^L;d=d+m|0;c=(I^K^C)+c|0;c=d>>>0<m>>>0?c+1|0:c;t=M(d,c,28);H=B;m=c;I=M(d,c,34);J=B;K=M(d,c,39);L=B;C=y|24;c=C+29792|0;w=f[c>>2];N=f[c+4>>2];c=k+(r^(r^s)&u)|0;k=i+(n^(n^o)&p)|0;if(k>>>0<i>>>0){c=c+1|0}i=v^D^F;k=i+k|0;c=(x^E^G)+c|0;c=k>>>0<i>>>0?c+1|0:c;i=k;k=i+w|0;c=c+N|0;c=k>>>0<i>>>0?c+1|0:c;i=z+C|0;v=f[i>>2];k=v+k|0;c=f[i+4>>2]+c|0;i=k;k=i>>>0<v>>>0?c+1|0:c;c=g+k|0;g=j+i|0;if(g>>>0<j>>>0){c=c+1|0}x=M(g,c,14);D=B;j=c;E=M(g,c,18);F=B;G=M(g,c,41);C=B;c=(e&(h|m)|h&m)+k|0;k=l&(d|q)|d&q;i=k+i|0;if(i>>>0<k>>>0){c=c+1|0}k=i;i=t^I^K;k=k+i|0;c=(H^J^L)+c|0;v=k;c=k>>>0<i>>>0?c+1|0:c;t=M(k,c,28);H=B;k=c;I=M(v,c,34);J=B;K=M(v,c,39);L=B;i=e;w=y|32;c=w+29792|0;e=f[c>>2];N=f[c+4>>2];O=e;c=r+(s^(s^u)&j)|0;e=n+(o^(p^o)&g)|0;if(e>>>0<n>>>0){c=c+1|0}n=x^E^G;e=n+e|0;c=(D^F^C)+c|0;c=e>>>0<n>>>0?c+1|0:c;n=e;e=O+e|0;c=c+N|0;c=e>>>0<n>>>0?c+1|0:c;n=z+w|0;r=f[n>>2];e=r+e|0;c=f[n+4>>2]+c|0;c=e>>>0<r>>>0?c+1|0:c;n=i;i=c;c=n+c|0;n=e+l|0;if(n>>>0<e>>>0){c=c+1|0}r=n;x=M(n,c,14);D=B;l=c;E=M(n,c,18);F=B;G=M(n,c,41);C=B;c=(h&(k|m)|k&m)+i|0;i=e;e=q&(d|v)|d&v;i=i+e|0;if(i>>>0<e>>>0){c=c+1|0}e=i;i=t^I^K;e=e+i|0;c=(H^J^L)+c|0;n=e;c=e>>>0<i>>>0?c+1|0:c;t=M(e,c,28);H=B;e=c;I=M(n,c,34);J=B;K=M(n,c,39);L=B;i=h;w=y|40;c=w+29792|0;h=f[c>>2];N=f[c+4>>2];O=h;c=s+(u^(j^u)&l)|0;h=o+(p^(g^p)&r)|0;if(h>>>0<o>>>0){c=c+1|0}o=x^E^G;h=o+h|0;c=(D^F^C)+c|0;c=h>>>0<o>>>0?c+1|0:c;o=h;h=O+h|0;c=c+N|0;c=h>>>0<o>>>0?c+1|0:c;o=z+w|0;s=f[o>>2];h=s+h|0;c=f[o+4>>2]+c|0;c=h>>>0<s>>>0?c+1|0:c;o=i;i=c;c=o+c|0;o=h+q|0;if(o>>>0<h>>>0){c=c+1|0}s=o;x=M(o,c,14);D=B;q=c;E=M(o,c,18);F=B;G=M(o,c,41);C=B;c=(m&(e|k)|e&k)+i|0;i=h;h=d&(n|v)|n&v;i=i+h|0;if(i>>>0<h>>>0){c=c+1|0}h=i;i=t^I^K;h=h+i|0;c=(H^J^L)+c|0;o=h;c=h>>>0<i>>>0?c+1|0:c;H=M(h,c,28);I=B;h=c;J=M(o,c,34);K=B;L=M(o,c,39);w=B;i=g;t=j;N=y|48;c=N+29792|0;O=f[c>>2];da=f[c+4>>2];c=u+(j^(j^l)&q)|0;j=p+(g^(g^r)&s)|0;if(j>>>0<p>>>0){c=c+1|0}g=x^E^G;j=g+j|0;c=(D^F^C)+c|0;c=j>>>0<g>>>0?c+1|0:c;g=j;j=g+O|0;c=c+da|0;c=j>>>0<g>>>0?c+1|0:c;g=z+N|0;p=f[g>>2];j=p+j|0;c=f[g+4>>2]+c|0;x=j;j=j>>>0<p>>>0?c+1|0:c;c=m+j|0;g=d+x|0;if(g>>>0<d>>>0){c=c+1|0}u=g;p=c;c=(l^c&(l^q))+t|0;g=r^(r^s)&g;d=g+i|0;if(d>>>0<g>>>0){c=c+1|0}g=d;d=M(u,p,14);m=B;d=M(u,p,18)^d;m=B^m;d=M(u,p,41)^d;g=d+g|0;c=(B^m)+c|0;c=g>>>0<d>>>0?c+1|0:c;d=y|56;m=d+29792|0;i=f[m>>2];g=i+g|0;c=f[m+4>>2]+c|0;c=g>>>0<i>>>0?c+1|0:c;d=d+z|0;m=f[d>>2];g=m+g|0;c=f[d+4>>2]+c|0;i=g;c=g>>>0<m>>>0?c+1|0:c;t=c;g=c;c=(k&(e|h)|e&h)+j|0;j=v&(n|o)|n&o;d=j+x|0;if(d>>>0<j>>>0){c=c+1|0}j=d;d=H^J^L;j=j+d|0;c=(I^K^w)+c|0;m=j;c=j>>>0<d>>>0?c+1|0:c;d=c;y=n&(o|j)|j&o;j=y+i|0;c=(e&(c|h)|c&h)+g|0;c=j>>>0<y>>>0?c+1|0:c;g=M(m,d,28);y=B;g=M(m,d,34)^g;y=B^y;g=M(m,d,39)^g;j=g+j|0;c=(B^y)+c|0;c=j>>>0<g>>>0?c+1|0:c;g=c;c=k+t|0;k=i+v|0;if(k>>>0<i>>>0){c=c+1|0}i=k;k=c;c=b>>>0<72;b=b+8|0;if(c){continue}break}c=l+Z|0;l=r+R|0;if(l>>>0<R>>>0){c=c+1|0}b=a;f[b+200>>2]=l;f[b+204>>2]=c;c=q+_|0;l=s+S|0;if(l>>>0<S>>>0){c=c+1|0}f[a+192>>2]=l;f[b+196>>2]=c;c=p+$|0;p=u+T|0;if(p>>>0<T>>>0){c=c+1|0}f[a+184>>2]=p;f[b+188>>2]=c;c=k+aa|0;k=i+U|0;if(k>>>0<U>>>0){c=c+1|0}f[a+176>>2]=k;f[b+180>>2]=c;c=e+ba|0;e=n+V|0;if(e>>>0<V>>>0){c=c+1|0}f[a+168>>2]=e;f[b+172>>2]=c;c=h+ca|0;e=o+W|0;if(e>>>0<W>>>0){c=c+1|0}f[a+160>>2]=e;f[b+164>>2]=c;c=d+Y|0;d=m+Q|0;if(d>>>0<Q>>>0){c=c+1|0}f[a+152>>2]=d;f[b+156>>2]=c;c=g+X|0;b=j+P|0;if(b>>>0<P>>>0){c=c+1|0}f[a+144>>2]=b;f[a+148>>2]=c;A=z+640|0}function rb(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,C=0,D=0;p=A-96|0;A=p;v=f[c>>2];w=f[b>>2];d=kc(v,0,w);h=B;f[p+48>>2]=d&1073741823;y=p;e=d;d=h>>>30|0;g=f[c+4>>2];e=(h&1073741823)<<2|e>>>30;h=kc(w,0,g)+e|0;d=d+B|0;d=h>>>0<e>>>0?d+1|0:d;j=f[b+4>>2];e=kc(v,0,j);h=e+h|0;d=B+d|0;x=h;h=h>>>0<e>>>0?d+1|0:d;f[y+52>>2]=x&1073741823;C=f[c+8>>2];d=kc(w,0,C);z=B;n=kc(j,0,g);e=n+d|0;d=B+z|0;d=e>>>0<n>>>0?d+1|0:d;z=f[b+8>>2];n=kc(v,0,z);e=n+e|0;d=B+d|0;d=e>>>0<n>>>0?d+1|0:d;n=e;e=d;d=h>>>30|0;x=(h&1073741823)<<2|x>>>30;h=x+n|0;d=d+e|0;q=h;h=h>>>0<x>>>0?d+1|0:d;f[y+56>>2]=q&1073741823;d=kc(g,0,z);x=B;n=kc(j,0,C);e=n+d|0;d=B+x|0;d=e>>>0<n>>>0?d+1|0:d;x=f[c+12>>2];n=kc(w,0,x);e=n+e|0;d=B+d|0;d=e>>>0<n>>>0?d+1|0:d;n=f[b+12>>2];r=kc(v,0,n);e=r+e|0;d=B+d|0;d=e>>>0<r>>>0?d+1|0:d;r=e;e=d;d=h>>>30|0;q=(h&1073741823)<<2|q>>>30;h=q+r|0;d=d+e|0;o=h;h=h>>>0<q>>>0?d+1|0:d;f[y+60>>2]=o&1073741823;d=kc(j,0,x);q=B;r=kc(C,0,z);e=r+d|0;d=B+q|0;d=e>>>0<r>>>0?d+1|0:d;q=kc(g,0,n);e=q+e|0;d=B+d|0;d=e>>>0<q>>>0?d+1|0:d;q=f[c+16>>2];r=kc(w,0,q);e=r+e|0;d=B+d|0;d=e>>>0<r>>>0?d+1|0:d;r=f[b+16>>2];s=kc(v,0,r);e=s+e|0;d=B+d|0;d=e>>>0<s>>>0?d+1|0:d;s=e;e=d;d=h>>>30|0;o=(h&1073741823)<<2|o>>>30;h=o+s|0;d=d+e|0;l=h;h=h>>>0<o>>>0?d+1|0:d;f[y+64>>2]=l&1073741823;d=kc(C,0,n);o=B;s=kc(x,0,z);e=s+d|0;d=B+o|0;d=e>>>0<s>>>0?d+1|0:d;o=kc(j,0,q);e=o+e|0;d=B+d|0;d=e>>>0<o>>>0?d+1|0:d;o=kc(g,0,r);e=o+e|0;d=B+d|0;d=e>>>0<o>>>0?d+1|0:d;o=f[c+20>>2];s=kc(w,0,o);e=s+e|0;d=B+d|0;d=e>>>0<s>>>0?d+1|0:d;s=f[b+20>>2];t=kc(v,0,s);e=t+e|0;d=B+d|0;d=e>>>0<t>>>0?d+1|0:d;t=e;e=d;d=h>>>30|0;l=(h&1073741823)<<2|l>>>30;h=l+t|0;d=d+e|0;k=h;h=h>>>0<l>>>0?d+1|0:d;f[y+68>>2]=k&1073741823;d=kc(q,0,z);l=B;t=kc(x,0,n);e=t+d|0;d=B+l|0;d=e>>>0<t>>>0?d+1|0:d;l=kc(C,0,r);e=l+e|0;d=B+d|0;d=e>>>0<l>>>0?d+1|0:d;l=kc(j,0,o);e=l+e|0;d=B+d|0;d=e>>>0<l>>>0?d+1|0:d;l=kc(g,0,s);e=l+e|0;d=B+d|0;d=e>>>0<l>>>0?d+1|0:d;l=f[c+24>>2];t=kc(w,0,l);e=t+e|0;d=B+d|0;d=e>>>0<t>>>0?d+1|0:d;t=f[b+24>>2];u=kc(v,0,t);e=u+e|0;d=B+d|0;d=e>>>0<u>>>0?d+1|0:d;u=e;e=d;d=h>>>30|0;k=(h&1073741823)<<2|k>>>30;h=k+u|0;d=d+e|0;i=h;h=h>>>0<k>>>0?d+1|0:d;f[y+72>>2]=i&1073741823;d=kc(x,0,r);k=B;u=kc(q,0,n);e=u+d|0;d=B+k|0;d=e>>>0<u>>>0?d+1|0:d;k=kc(o,0,z);e=k+e|0;d=B+d|0;d=e>>>0<k>>>0?d+1|0:d;k=kc(C,0,s);e=k+e|0;d=B+d|0;d=e>>>0<k>>>0?d+1|0:d;k=kc(j,0,l);e=k+e|0;d=B+d|0;d=e>>>0<k>>>0?d+1|0:d;k=kc(g,0,t);e=k+e|0;d=B+d|0;d=e>>>0<k>>>0?d+1|0:d;k=f[c+28>>2];u=kc(w,0,k);e=u+e|0;d=B+d|0;d=e>>>0<u>>>0?d+1|0:d;u=f[b+28>>2];m=kc(v,0,u);e=m+e|0;d=B+d|0;d=e>>>0<m>>>0?d+1|0:d;m=e;e=d;d=h>>>30|0;i=(h&1073741823)<<2|i>>>30;h=i+m|0;d=d+e|0;e=h;h=e>>>0<i>>>0?d+1|0:d;f[y+76>>2]=e&1073741823;d=kc(o,0,n);m=B;D=kc(q,0,r);i=D+d|0;d=B+m|0;d=i>>>0<D>>>0?d+1|0:d;m=kc(x,0,s);i=m+i|0;d=B+d|0;d=i>>>0<m>>>0?d+1|0:d;m=kc(l,0,z);i=m+i|0;d=B+d|0;d=i>>>0<m>>>0?d+1|0:d;m=kc(C,0,t);i=m+i|0;d=B+d|0;d=i>>>0<m>>>0?d+1|0:d;m=kc(j,0,k);i=m+i|0;d=B+d|0;d=i>>>0<m>>>0?d+1|0:d;m=kc(g,0,u);i=m+i|0;d=B+d|0;d=i>>>0<m>>>0?d+1|0:d;m=i;i=w;w=f[c+32>>2];i=kc(i,0,w);c=m+i|0;d=B+d|0;d=c>>>0<i>>>0?d+1|0:d;i=c;c=v;v=f[b+32>>2];c=kc(c,0,v);b=i+c|0;d=B+d|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;d=h>>>30|0;h=(h&1073741823)<<2|e>>>30;c=h+c|0;d=b+d|0;d=c>>>0<h>>>0?d+1|0:d;b=d;h=c;f[y+80>>2]=c&16777215;d=kc(q,0,s);i=B;m=kc(o,0,r);e=m+d|0;d=B+i|0;d=e>>>0<m>>>0?d+1|0:d;i=kc(l,0,n);e=i+e|0;d=B+d|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(x,0,t);e=i+e|0;d=B+d|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(k,0,z);e=i+e|0;d=B+d|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(C,0,u);e=i+e|0;d=B+d|0;d=e>>>0<i>>>0?d+1|0:d;j=kc(j,0,w);e=j+e|0;d=B+d|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(v,0,g);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;y=c;f[p>>2]=c<<22&1069547520|h>>>8&4194303;d=kc(l,0,r);g=B;j=kc(o,0,s);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(q,0,t);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(k,0,n);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(x,0,u);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(w,0,z);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(v,0,C);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;h=c;f[p+4>>2]=c<<22&1069547520|y>>>8&4194303;d=kc(o,0,t);g=B;j=kc(l,0,s);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(k,0,r);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(q,0,u);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(w,0,n);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(v,0,x);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;y=c;f[p+8>>2]=c<<22&1069547520|h>>>8&4194303;d=kc(k,0,s);g=B;j=kc(l,0,t);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(o,0,u);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(w,0,r);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(v,0,q);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;h=c;f[p+12>>2]=c<<22&1069547520|y>>>8&4194303;d=kc(l,0,u);g=B;j=kc(k,0,t);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(w,0,s);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=kc(v,0,o);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;y=c;f[p+16>>2]=c<<22&1069547520|h>>>8&4194303;d=kc(w,0,t);g=B;j=kc(k,0,u);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;g=kc(v,0,l);e=g+e|0;d=B+d|0;d=e>>>0<g>>>0?d+1|0:d;g=e;e=d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;b=d;h=c;f[p+20>>2]=c<<22&1069547520|y>>>8&4194303;d=kc(v,0,k);g=B;j=kc(w,0,u);e=j+d|0;d=B+g|0;g=e;e=e>>>0<j>>>0?d+1|0:d;d=b>>>30|0;c=(b&1073741823)<<2|c>>>30;b=c+g|0;d=d+e|0;d=b>>>0<c>>>0?d+1|0:d;c=b;f[p+24>>2]=c<<22&1069547520|h>>>8&4194303;b=kc(v,0,w)+((d&1073741823)<<2|c>>>30)|0;f[p+32>>2]=b>>>8&4194303;f[p+28>>2]=b<<22&1069547520|c>>>8&4194303;Ma(a,p,p+48|0);A=p+96|0}function mb(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0;d=A-336|0;A=d;i=f[b+40>>2];j=f[b>>2];e=(i-j|0)+134217690|0;f[d+288>>2]=e&67108863;k=f[b+44>>2];m=f[b+4>>2];e=((k+(e>>>26|0)|0)-m|0)+67108862|0;f[d+292>>2]=e&33554431;n=f[b+48>>2];o=f[b+8>>2];e=(n-o+(e>>>25)|0)+134217726|0;f[d+296>>2]=e&67108863;t=f[b+52>>2];q=f[b+12>>2];e=(t-q+(e>>>26)|0)+67108862|0;f[d+300>>2]=e&33554431;p=f[b+56>>2];g=f[b+16>>2];f[d+304>>2]=(p-g+(e>>>25)|0)+134217726;u=f[b+60>>2];v=f[b+20>>2];f[d+308>>2]=(u-v|0)+67108862;w=f[b- -64>>2];r=f[b+24>>2];f[d+312>>2]=(w-r|0)+134217726;s=f[b+68>>2];x=f[b+28>>2];f[d+316>>2]=(s-x|0)+67108862;y=f[b+72>>2];z=f[b+32>>2];f[d+320>>2]=(y-z|0)+134217726;e=f[b+76>>2];h=f[b+36>>2];f[d+272>>2]=y+z;f[d+268>>2]=s+x;f[d+264>>2]=w+r;f[d+260>>2]=u+v;f[d+256>>2]=g+p;f[d+252>>2]=t+q;f[d+248>>2]=n+o;f[d+244>>2]=k+m;f[d+240>>2]=i+j;f[d+276>>2]=e+h;f[d+324>>2]=(e-h|0)+67108862;N(d+288|0,d+288|0,c);N(d+144|0,d+240|0,c+40|0);e=f[d+288>>2];h=f[d+144>>2];f[d+96>>2]=e+h;i=f[d+292>>2];j=f[d+148>>2];f[d+100>>2]=i+j;k=f[d+296>>2];m=f[d+152>>2];f[d+104>>2]=k+m;n=f[d+300>>2];o=f[d+156>>2];f[d+108>>2]=n+o;t=f[d+304>>2];q=f[d+160>>2];f[d+112>>2]=t+q;p=f[d+308>>2];g=f[d+164>>2];f[d+116>>2]=p+g;f[d+164>>2]=(g-p|0)+67108862;e=(h-e|0)+134217690|0;f[d+144>>2]=e&67108863;h=f[d+312>>2];p=f[d+168>>2];f[d+120>>2]=h+p;g=f[d+316>>2];u=f[d+172>>2];f[d+124>>2]=g+u;v=f[d+320>>2];w=f[d+176>>2];f[d+128>>2]=v+w;r=f[d+324>>2];s=f[d+180>>2];f[d+132>>2]=r+s;f[d+172>>2]=(u-g|0)+67108862;f[d+176>>2]=(w-v|0)+134217726;f[d+180>>2]=(s-r|0)+67108862;f[d+168>>2]=(p-h|0)+134217726;e=((j+(e>>>26|0)|0)-i|0)+67108862|0;f[d+148>>2]=e&33554431;e=(m-k+(e>>>25)|0)+134217726|0;f[d+152>>2]=e&67108863;e=(o-n+(e>>>26)|0)+67108862|0;f[d+156>>2]=e&33554431;f[d+160>>2]=(q-t+(e>>>25)|0)+134217726;N(d+192|0,b+120|0,c+120|0);N(d,b+80|0,c+80|0);b=f[d+36>>2]<<1;f[d+36>>2]=b;c=f[d+32>>2]<<1;f[d+32>>2]=c;e=f[d+28>>2]<<1;f[d+28>>2]=e;h=f[d+24>>2]<<1;f[d+24>>2]=h;i=f[d+20>>2]<<1;f[d+20>>2]=i;j=f[d+16>>2]<<1;f[d+16>>2]=j;k=f[d+12>>2]<<1;f[d+12>>2]=k;m=f[d+8>>2]<<1;f[d+8>>2]=m;n=f[d+4>>2]<<1;f[d+4>>2]=n;o=f[d>>2]<<1;f[d>>2]=o;p=f[d+196>>2];t=f[d+192>>2];q=t+o|0;g=p+(n+(q>>>26|0)|0)|0;f[d+52>>2]=g&33554431;u=f[d+200>>2];g=u+m+(g>>>25)|0;f[d+56>>2]=g&67108863;v=f[d+204>>2];g=v+k+(g>>>26)|0;f[d+60>>2]=g&33554431;w=f[d+208>>2];g=w+j+(g>>>25)|0;f[d+64>>2]=g&67108863;r=f[d+212>>2];g=r+i+(g>>>26)|0;f[d+68>>2]=g&33554431;s=f[d+216>>2];g=s+h+(g>>>25)|0;f[d+72>>2]=g&67108863;x=f[d+220>>2];g=x+e+(g>>>26)|0;f[d+76>>2]=g&33554431;y=f[d+224>>2];g=y+c+(g>>>25)|0;f[d+80>>2]=g&67108863;z=f[d+228>>2];g=z+b+(g>>>26)|0;f[d+84>>2]=g&33554431;f[d+48>>2]=l(g>>>25|0,19)+(q&67108863);g=b-z|0;q=c-y|0;x=e-x|0;s=h-s|0;r=i-r|0;b=(o-t|0)+268435380|0;c=((n+(b>>>26|0)|0)-p|0)+134217724|0;e=(m-u+(c>>>25)|0)+268435452|0;h=(k-v+(e>>>26)|0)+134217724|0;i=(j-w+(h>>>25)|0)+268435452|0;j=(r+(i>>>26|0)|0)+134217724|0;k=(s+(j>>>25|0)|0)+268435452|0;m=(x+(k>>>26|0)|0)+134217724|0;n=(q+(m>>>25|0)|0)+268435452|0;o=(g+(n>>>26|0)|0)+134217724|0;f[d+36>>2]=o&33554431;f[d+32>>2]=n&67108863;f[d+28>>2]=m&33554431;f[d+24>>2]=k&67108863;f[d+20>>2]=j&33554431;f[d+16>>2]=i&67108863;f[d+12>>2]=h&33554431;f[d+8>>2]=e&67108863;f[d+4>>2]=c&33554431;f[d>>2]=l(o>>>25|0,19)+(b&67108863);N(a+40|0,d+144|0,d);N(a,d+96|0,d+48|0);N(a+80|0,d+48|0,d);b=a+120|0;N(b,d+144|0,d+96|0);j=f[a>>2];k=f[a+40>>2];c=(j-k|0)+134217690|0;f[a>>2]=c&67108863;m=f[a+4>>2];n=f[a+44>>2];c=((m+(c>>>26|0)|0)-n|0)+67108862|0;f[a+4>>2]=c&33554431;o=f[a+8>>2];t=f[a+48>>2];c=(o-t+(c>>>25)|0)+134217726|0;f[a+8>>2]=c&67108863;q=f[a+12>>2];p=f[a+52>>2];c=(q-p+(c>>>26)|0)+67108862|0;f[a+12>>2]=c&33554431;g=f[a+16>>2];u=f[a+56>>2];f[a+16>>2]=(g-u+(c>>>25)|0)+134217726;c=f[a+36>>2];e=f[a+32>>2];h=f[a+28>>2];i=f[a+24>>2];v=f[a+20>>2];w=f[a+60>>2];f[a+20>>2]=(v-w|0)+67108862;r=a- -64|0;s=f[r>>2];f[a+24>>2]=(i-s|0)+134217726;x=f[a+68>>2];f[a+28>>2]=(h-x|0)+67108862;y=f[a+72>>2];f[a+32>>2]=(e-y|0)+134217726;f[a+56>>2]=g+u;f[a+44>>2]=m+n;f[a+48>>2]=o+t;f[a+52>>2]=p+q;f[a+60>>2]=v+w;f[r>>2]=i+s;f[a+68>>2]=h+x;f[a+72>>2]=e+y;e=f[a+76>>2];f[a+40>>2]=j+k;f[a+76>>2]=c+e;f[a+36>>2]=(c-e|0)+67108862;N(b,b,29616);A=d+336|0}function gb(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0;d=A-288|0;A=d;m=f[b+40>>2];n=f[b>>2];g=(m-n|0)+134217690|0;f[d+240>>2]=g&67108863;o=f[b+44>>2];p=f[b+4>>2];g=((o+(g>>>26|0)|0)-p|0)+67108862|0;f[d+244>>2]=g&33554431;q=f[b+48>>2];r=f[b+8>>2];g=(q-r+(g>>>25)|0)+134217726|0;f[d+248>>2]=g&67108863;e=f[b+52>>2];t=f[b+12>>2];g=(e-t+(g>>>26)|0)+67108862|0;f[d+252>>2]=g&33554431;u=f[b+56>>2];v=f[b+16>>2];f[d+256>>2]=(u-v+(g>>>25)|0)+134217726;w=f[b+60>>2];x=f[b+20>>2];f[d+260>>2]=(w-x|0)+67108862;y=f[b- -64>>2];h=f[b+24>>2];f[d+264>>2]=(y-h|0)+134217726;i=f[b+68>>2];j=f[b+28>>2];f[d+268>>2]=(i-j|0)+67108862;k=f[b+72>>2];z=f[b+32>>2];f[d+272>>2]=(k-z|0)+134217726;g=f[b+76>>2];s=f[b+36>>2];f[d+224>>2]=k+z;f[d+220>>2]=i+j;f[d+216>>2]=y+h;f[d+212>>2]=w+x;f[d+208>>2]=u+v;f[d+204>>2]=e+t;f[d+200>>2]=q+r;f[d+196>>2]=o+p;f[d+192>>2]=m+n;f[d+228>>2]=g+s;f[d+276>>2]=(g-s|0)+67108862;m=f[c+40>>2];n=f[c>>2];g=(m-n|0)+134217690|0;f[d+48>>2]=g&67108863;o=f[c+44>>2];p=f[c+4>>2];g=((o+(g>>>26|0)|0)-p|0)+67108862|0;f[d+52>>2]=g&33554431;q=f[c+48>>2];r=f[c+8>>2];g=(q-r+(g>>>25)|0)+134217726|0;f[d+56>>2]=g&67108863;e=f[c+52>>2];t=f[c+12>>2];g=(e-t+(g>>>26)|0)+67108862|0;f[d+60>>2]=g&33554431;u=f[c+56>>2];v=f[c+16>>2];f[d+64>>2]=(u-v+(g>>>25)|0)+134217726;w=f[c+60>>2];x=f[c+20>>2];f[d+68>>2]=(w-x|0)+67108862;y=f[c- -64>>2];h=f[c+24>>2];f[d+72>>2]=(y-h|0)+134217726;i=f[c+68>>2];j=f[c+28>>2];f[d+76>>2]=(i-j|0)+67108862;k=f[c+72>>2];z=f[c+32>>2];f[d+80>>2]=(k-z|0)+134217726;g=f[c+76>>2];s=f[c+36>>2];f[d+32>>2]=k+z;f[d+28>>2]=i+j;f[d+24>>2]=y+h;f[d+20>>2]=w+x;f[d+16>>2]=u+v;f[d+12>>2]=e+t;f[d+8>>2]=q+r;f[d+4>>2]=o+p;f[d>>2]=m+n;f[d+36>>2]=g+s;f[d+84>>2]=(g-s|0)+67108862;N(d+240|0,d+240|0,d+48|0);N(d+192|0,d+192|0,d);N(d+144|0,b+120|0,c+120|0);N(d+144|0,d+144|0,29616);N(d+96|0,b+80|0,c+80|0);b=f[d+132>>2]<<1;f[d+132>>2]=b;c=f[d+128>>2]<<1;f[d+128>>2]=c;g=f[d+124>>2]<<1;f[d+124>>2]=g;s=f[d+120>>2]<<1;f[d+120>>2]=s;m=f[d+116>>2]<<1;f[d+116>>2]=m;n=f[d+112>>2]<<1;f[d+112>>2]=n;o=f[d+108>>2]<<1;f[d+108>>2]=o;p=f[d+104>>2]<<1;f[d+104>>2]=p;q=f[d+100>>2]<<1;f[d+100>>2]=q;r=f[d+96>>2]<<1;f[d+96>>2]=r;u=f[d+192>>2];v=f[d+240>>2];e=(u-v|0)+134217690|0;f[a>>2]=e&67108863;w=f[d+196>>2];x=f[d+244>>2];e=((w+(e>>>26|0)|0)-x|0)+67108862|0;f[a+4>>2]=e&33554431;y=f[d+200>>2];h=f[d+248>>2];e=(y-h+(e>>>25)|0)+134217726|0;f[a+8>>2]=e&67108863;i=f[d+204>>2];j=f[d+252>>2];e=(i-j+(e>>>26)|0)+67108862|0;f[a+12>>2]=e&33554431;k=f[d+208>>2];z=f[d+256>>2];f[a+16>>2]=(k-z+(e>>>25)|0)+134217726;B=f[d+212>>2];C=f[d+260>>2];f[a+20>>2]=(B-C|0)+67108862;D=f[d+216>>2];E=f[d+264>>2];f[a+24>>2]=(D-E|0)+134217726;F=f[d+220>>2];G=f[d+268>>2];f[a+28>>2]=(F-G|0)+67108862;H=f[d+224>>2];I=f[d+272>>2];f[a+32>>2]=(H-I|0)+134217726;e=f[d+276>>2];t=f[d+228>>2];f[a+72>>2]=H+I;f[a+68>>2]=F+G;f[a- -64>>2]=D+E;f[a+60>>2]=B+C;f[a+56>>2]=k+z;f[a+52>>2]=i+j;f[a+48>>2]=y+h;f[a+44>>2]=w+x;f[a+40>>2]=u+v;f[a+76>>2]=e+t;f[a+36>>2]=(t-e|0)+67108862;v=f[d+148>>2];t=f[d+144>>2];u=t+r|0;e=v+(q+(u>>>26|0)|0)|0;f[a+84>>2]=e&33554431;w=f[d+152>>2];e=w+p+(e>>>25)|0;f[a+88>>2]=e&67108863;x=f[d+156>>2];e=x+o+(e>>>26)|0;f[a+92>>2]=e&33554431;y=f[d+160>>2];e=y+n+(e>>>25)|0;f[a+96>>2]=e&67108863;h=f[d+164>>2];e=h+m+(e>>>26)|0;f[a+100>>2]=e&33554431;i=f[d+168>>2];e=i+s+(e>>>25)|0;f[a+104>>2]=e&67108863;j=f[d+172>>2];e=j+g+(e>>>26)|0;f[a+108>>2]=e&33554431;k=f[d+176>>2];z=k+c+(e>>>25)|0;f[a+112>>2]=z&67108863;e=f[d+180>>2];k=c-k|0;j=g-j|0;i=s-i|0;h=m-h|0;c=(r-t|0)+268435380|0;g=((q+(c>>>26|0)|0)-v|0)+134217724|0;s=(p-w+(g>>>25)|0)+268435452|0;m=(o-x+(s>>>26)|0)+134217724|0;n=(n-y+(m>>>25)|0)+268435452|0;o=(h+(n>>>26|0)|0)+134217724|0;p=(i+(o>>>25|0)|0)+268435452|0;q=(j+(p>>>26|0)|0)+134217724|0;r=(k+(q>>>25|0)|0)+268435452|0;f[a+152>>2]=r&67108863;f[a+148>>2]=q&33554431;f[a+144>>2]=p&67108863;f[a+140>>2]=o&33554431;f[a+136>>2]=n&67108863;f[a+132>>2]=m&33554431;f[a+128>>2]=s&67108863;f[a+124>>2]=g&33554431;g=b+e+(z>>>26)|0;f[a+116>>2]=g&33554431;b=(b-e+(r>>>26)|0)+134217724|0;f[a+156>>2]=b&33554431;f[a+80>>2]=l(g>>>25|0,19)+(u&67108863);f[a+120>>2]=l(b>>>25|0,19)+(c&67108863);A=d+288|0}function T(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0;i=f[b+36>>2];k=f[b+32>>2];m=f[b+28>>2];j=f[b+24>>2];g=f[b+20>>2];h=f[b+16>>2];w=f[b+12>>2];q=f[b+8>>2];y=f[b+4>>2];b=f[b>>2];while(1){z=g<<1;D=l(j,19);d=kc(z,0,D);p=B;n=h;C=l(m,38);o=kc(h,0,C);e=o+d|0;d=B+p|0;d=e>>>0<o>>>0?d+1|0:d;x=w<<1;A=l(k,19);o=kc(x,0,A);p=o+e|0;e=B+d|0;e=p>>>0<o>>>0?e+1|0:e;d=p;s=l(i,38);p=kc(s,0,q&2147483647);d=d+p|0;e=B+e|0;e=d>>>0<p>>>0?e+1|0:e;t=b<<1;G=y;o=kc(t,0,y);p=o+d|0;d=B+e|0;r=p;o=p>>>0<o>>>0?d+1|0:d;E=h<<1;d=kc(E,0,D);e=B;p=g;h=kc(g,0,l(g,38));d=h+d|0;g=B+e|0;g=d>>>0<h>>>0?g+1|0:g;h=kc(x,0,C);e=h+d|0;d=B+g|0;d=e>>>0<h>>>0?d+1|0:d;u=q<<1;h=kc(u,0,A);g=h+e|0;e=B+d|0;e=g>>>0<h>>>0?e+1|0:e;d=g;v=y<<1;g=kc(v,0,s);d=d+g|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;g=kc(b,0,b);b=g+d|0;d=B+e|0;d=b>>>0<g>>>0?d+1|0:d;y=b;e=d>>>26|0;d=(d&67108863)<<6|b>>>26;b=d+r|0;g=e+o|0;F=b;b=b>>>0<d>>>0?g+1|0:g;h=j;d=kc(x,0,h);g=B;o=kc(n,0,z);e=o+d|0;d=B+g|0;d=e>>>0<o>>>0?d+1|0:d;o=m;r=kc(u,0,m);g=r+e|0;e=B+d|0;e=g>>>0<r>>>0?e+1|0:e;r=kc(v,0,k);g=r+g|0;d=B+e|0;d=g>>>0<r>>>0?d+1|0:d;r=kc(t,0,i);g=r+g|0;e=B+d|0;H=g;r=g>>>0<r>>>0?e+1|0:e;d=kc(n,0,n);e=B;i=kc(s,0,i);d=i+d|0;g=B+e|0;g=d>>>0<i>>>0?g+1|0:g;i=kc(x,0,z);e=i+d|0;d=B+g|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(u,0,h);g=i+e|0;e=B+d|0;e=g>>>0<i>>>0?e+1|0:e;m=m<<1;i=kc(v,0,m);g=i+g|0;d=B+e|0;d=g>>>0<i>>>0?d+1|0:d;i=kc(t,0,k);g=i+g|0;e=B+d|0;I=g;J=g>>>0<i>>>0?e+1|0:e;d=kc(n,0,x);e=B;i=kc(s,0,k);d=i+d|0;g=B+e|0;g=d>>>0<i>>>0?g+1|0:g;i=kc(u,0,p);e=i+d|0;d=B+g|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(v,0,h);g=i+e|0;e=B+d|0;e=g>>>0<i>>>0?e+1|0:e;i=kc(t,0,o);g=i+g|0;d=B+e|0;K=g;i=g>>>0<i>>>0?d+1|0:d;d=kc(s,0,m);e=B;g=kc(A,0,k);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;k=kc(x,0,w);d=k+d|0;g=B+e|0;g=d>>>0<k>>>0?g+1|0:g;k=kc(u,0,n);e=k+d|0;d=B+g|0;d=e>>>0<k>>>0?d+1|0:d;k=kc(v,0,z);g=k+e|0;e=B+d|0;e=g>>>0<k>>>0?e+1|0:e;k=kc(t,0,h);g=k+g|0;d=B+e|0;L=g;k=g>>>0<k>>>0?d+1|0:d;d=kc(s,0,h);e=B;g=kc(A,0,m);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;m=kc(u,0,w);d=m+d|0;g=B+e|0;g=d>>>0<m>>>0?g+1|0:g;m=kc(n,0,v);e=m+d|0;d=B+g|0;d=e>>>0<m>>>0?d+1|0:d;m=kc(t,0,p);g=m+e|0;e=B+d|0;p=g;m=g>>>0<m>>>0?e+1|0:e;d=kc(h<<1,0,A);g=B;j=kc(C,0,o);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;j=kc(s,0,z);g=j+e|0;e=B+d|0;e=g>>>0<j>>>0?e+1|0:e;j=kc(q,0,q);d=j+g|0;g=B+e|0;g=d>>>0<j>>>0?g+1|0:g;j=kc(v,0,x);e=j+d|0;d=B+g|0;d=e>>>0<j>>>0?d+1|0:d;j=kc(n,0,t);g=j+e|0;e=B+d|0;o=g;j=g>>>0<j>>>0?e+1|0:e;d=kc(z,0,A);g=B;u=kc(h,0,C);e=u+d|0;d=B+g|0;n=kc(n,0,s);g=n+e|0;e=B+(e>>>0<u>>>0?d+1|0:d)|0;e=g>>>0<n>>>0?e+1|0:e;n=kc(q,0,v);d=n+g|0;g=B+e|0;g=d>>>0<n>>>0?g+1|0:g;n=kc(t,0,w);e=n+d|0;d=B+g|0;w=e;n=e>>>0<n>>>0?d+1|0:d;d=kc(z,0,C);e=B;g=kc(h,0,D);d=g+d|0;e=B+e|0;e=d>>>0<g>>>0?e+1|0:e;h=kc(A,0,E);g=h+d|0;d=B+e|0;d=g>>>0<h>>>0?d+1|0:d;h=kc(s,0,x);g=h+g|0;e=B+d|0;e=g>>>0<h>>>0?e+1|0:e;h=kc(v,0,G);d=h+g|0;g=B+e|0;g=d>>>0<h>>>0?g+1|0:g;h=kc(q,0,t);e=h+d|0;d=B+g|0;d=e>>>0<h>>>0?d+1|0:d;h=e;e=b>>>25|0;g=(b&33554431)<<7|F>>>25;b=h+g|0;e=d+e|0;e=b>>>0<g>>>0?e+1|0:e;g=b;d=e>>>26|0;g=(e&67108863)<<6|g>>>26;e=g+w|0;d=d+n|0;d=e>>>0<g>>>0?d+1|0:d;n=e;g=e;e=d>>>25|0;h=(d&33554431)<<7|g>>>25;d=h+o|0;g=e+j|0;g=d>>>0<h>>>0?g+1|0:g;h=d;e=d;d=g>>>26|0;j=(g&67108863)<<6|e>>>26;g=j+p|0;e=d+m|0;m=g;d=g;e=d>>>0<j>>>0?e+1|0:e;g=e>>>25|0;j=(e&33554431)<<7|d>>>25;e=j+L|0;d=g+k|0;d=e>>>0<j>>>0?d+1|0:d;j=e;g=e;e=d>>>26|0;g=(d&67108863)<<6|g>>>26;d=g+K|0;e=e+i|0;e=d>>>0<g>>>0?e+1|0:e;i=d;g=d;d=e>>>25|0;g=(e&33554431)<<7|g>>>25;e=g+I|0;d=d+J|0;d=e>>>0<g>>>0?d+1|0:d;k=e;g=e;e=d>>>26|0;q=(d&67108863)<<6|g>>>26;d=q+H|0;g=e+r|0;g=d>>>0<q>>>0?g+1|0:g;q=y&67108863;g=kc((g&33554431)<<7|d>>>25,0,19)+q|0;e=B;e=g>>>0<q>>>0?e+1|0:e;p=g;y=((e&67108863)<<6|g>>>26)+(F&33554431)|0;q=b&67108863;w=n&33554431;h=h&67108863;g=m&33554431;j=j&67108863;m=i&33554431;k=k&67108863;i=d&33554431;b=p&67108863;c=c+ -1|0;if(c){continue}break}f[a+36>>2]=i;f[a+32>>2]=k;f[a+28>>2]=m;f[a+24>>2]=j;f[a+20>>2]=g;f[a+16>>2]=h;f[a+12>>2]=w;f[a+8>>2]=q;f[a+4>>2]=y;f[a>>2]=b}function Y(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0;e=a;p=f[b+8>>2];D=p;k=f[b>>2];r=k<<1;a=kc(p,0,r);q=B;c=a;a=f[b+4>>2];s=a<<1;j=a;h=kc(s,0,a);c=c+h|0;a=B+q|0;a=c>>>0<h>>>0?a+1|0:a;h=f[b+24>>2];z=l(h,19);q=h;m=kc(z,0,h);c=m+c|0;d=B+a|0;d=c>>>0<m>>>0?d+1|0:d;a=c;m=f[b+28>>2];A=l(m,38);c=f[b+20>>2];w=c<<1;u=kc(A,0,w);g=a+u|0;a=B+d|0;a=g>>>0<u>>>0?a+1|0:a;o=g;u=f[b+32>>2];x=l(u,19);g=f[b+16>>2];C=g<<1;i=kc(x,0,C);y=o+i|0;a=B+a|0;a=y>>>0<i>>>0?a+1|0:a;o=y;y=f[b+36>>2];t=l(y,38);i=f[b+12>>2];v=i<<1;n=kc(t,0,v);o=o+n|0;b=B+a|0;E=o;o=o>>>0<n>>>0?b+1|0:b;b=kc(z,0,w);a=B;j=kc(r,0,j);b=j+b|0;a=B+a|0;a=b>>>0<j>>>0?a+1|0:a;j=g;g=kc(A,0,g);b=g+b|0;d=B+a|0;d=b>>>0<g>>>0?d+1|0:d;g=kc(x,0,v);b=g+b|0;a=B+d|0;a=b>>>0<g>>>0?a+1|0:a;g=kc(t,0,p&2147483647);b=g+b|0;a=B+a|0;a=b>>>0<g>>>0?a+1|0:a;n=a;g=c;a=kc(c,0,l(c,38));c=B;F=b;k=kc(k,0,k);a=k+a|0;b=B+c|0;b=a>>>0<k>>>0?b+1|0:b;k=kc(z,0,C);c=k+a|0;a=B+b|0;a=c>>>0<k>>>0?a+1|0:a;b=c;c=kc(A,0,v);b=b+c|0;d=B+a|0;d=b>>>0<c>>>0?d+1|0:d;p=p<<1;c=kc(x,0,p);b=c+b|0;a=B+d|0;a=b>>>0<c>>>0?a+1|0:a;c=kc(t,0,s);b=c+b|0;a=B+a|0;a=b>>>0<c>>>0?a+1|0:a;z=b;d=a>>>26|0;c=(a&67108863)<<6|b>>>26;a=F+c|0;b=d+n|0;b=a>>>0<c>>>0?b+1|0:b;C=a;c=a;a=b>>>25|0;c=(b&33554431)<<7|c>>>25;b=c+E|0;a=a+o|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[e+8>>2]=b&67108863;k=e;e=i;b=kc(e,0,r);a=B;i=kc(s,0,D);b=i+b|0;a=B+a|0;a=b>>>0<i>>>0?a+1|0:a;i=kc(A,0,h);b=i+b|0;d=B+a|0;d=b>>>0<i>>>0?d+1|0:d;i=kc(x,0,w);a=i+b|0;b=B+d|0;b=a>>>0<i>>>0?b+1|0:b;n=kc(j,0,t);i=n+a|0;a=B+b|0;d=c>>>26|0;c=(c&67108863)<<6|o>>>26;b=c+i|0;a=d+(i>>>0<n>>>0?a+1|0:a)|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[k+12>>2]=b&33554431;i=k;b=kc(s,0,v);a=B;k=kc(D,0,D);b=k+b|0;a=B+a|0;a=b>>>0<k>>>0?a+1|0:a;n=kc(r,0,j);k=n+b|0;b=B+a|0;b=k>>>0<n>>>0?b+1|0:b;a=k;k=m;d=kc(A,0,k);n=a+d|0;a=B+b|0;h=kc(x,0,h<<1);b=h+n|0;d=B+(n>>>0<d>>>0?a+1|0:a)|0;d=b>>>0<h>>>0?d+1|0:d;h=kc(t,0,w);b=h+b|0;a=B+d|0;a=b>>>0<h>>>0?a+1|0:a;h=b;b=a;m=h;a=c>>>25|0;h=(c&33554431)<<7|o>>>25;c=m+h|0;a=b+a|0;o=c;c=c>>>0<h>>>0?a+1|0:a;f[i+16>>2]=o&67108863;h=i;b=kc(j,0,s);i=B;n=kc(p,0,e);a=n+b|0;b=B+i|0;b=a>>>0<n>>>0?b+1|0:b;i=kc(r,0,g);a=i+a|0;d=B+b|0;d=a>>>0<i>>>0?d+1|0:d;i=k<<1;m=kc(x,0,i);b=m+a|0;a=B+d|0;a=b>>>0<m>>>0?a+1|0:a;m=kc(t,0,q);b=m+b|0;a=B+a|0;a=b>>>0<m>>>0?a+1|0:a;d=c>>>26|0;c=(c&67108863)<<6|o>>>26;b=c+b|0;a=a+d|0;o=b;c=b>>>0<c>>>0?a+1|0:a;f[h+20>>2]=b&33554431;m=h;b=kc(v,0,e);h=B;e=kc(j,0,p);a=e+b|0;b=B+h|0;b=a>>>0<e>>>0?b+1|0:b;e=kc(s,0,w);h=e+a|0;a=B+b|0;a=h>>>0<e>>>0?a+1|0:a;b=h;h=kc(r,0,q);b=b+h|0;a=B+a|0;a=b>>>0<h>>>0?a+1|0:a;h=u;e=kc(x,0,h);b=e+b|0;d=B+a|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(t,0,i);b=e+b|0;a=B+d|0;a=b>>>0<e>>>0?a+1|0:a;e=b;b=a;n=e;a=c>>>25|0;e=(c&33554431)<<7|o>>>25;c=n+e|0;b=b+a|0;b=c>>>0<e>>>0?b+1|0:b;f[m+24>>2]=c&67108863;a=kc(p,0,g);e=B;u=kc(j,0,v);g=u+a|0;a=B+e|0;a=g>>>0<u>>>0?a+1|0:a;e=kc(s,0,q);g=e+g|0;d=B+a|0;d=g>>>0<e>>>0?d+1|0:d;e=kc(r,0,k);g=e+g|0;a=B+d|0;a=g>>>0<e>>>0?a+1|0:a;e=kc(h,0,t);g=e+g|0;a=B+a|0;a=g>>>0<e>>>0?a+1|0:a;e=g;g=(b&67108863)<<6|c>>>26;c=e+g|0;b=(b>>>26|0)+a|0;b=c>>>0<g>>>0?b+1|0:b;g=c;c=b;f[m+28>>2]=g&33554431;b=kc(p,0,q);a=B;e=kc(j,0,j);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=kc(v,0,w);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=kc(s,0,i);b=e+b|0;a=B+a|0;a=b>>>0<e>>>0?a+1|0:a;e=kc(r,0,h);b=e+b|0;d=B+a|0;d=b>>>0<e>>>0?d+1|0:d;e=kc(t,0,y);a=e+b|0;b=B+d|0;b=a>>>0<e>>>0?b+1|0:b;e=a;a=c>>>25|0;g=(c&33554431)<<7|g>>>25;c=e+g|0;a=b+a|0;a=c>>>0<g>>>0?a+1|0:a;g=c;c=a;f[m+32>>2]=g&67108863;b=kc(j,0,w);a=B;j=kc(v,0,q);b=j+b|0;a=B+a|0;a=b>>>0<j>>>0?a+1|0:a;j=kc(p,0,k);b=j+b|0;d=B+a|0;d=b>>>0<j>>>0?d+1|0:d;j=kc(h,0,s);a=j+b|0;b=B+d|0;b=a>>>0<j>>>0?b+1|0:b;q=kc(r,0,y);j=q+a|0;a=B+b|0;d=c>>>26|0;c=(c&67108863)<<6|g>>>26;b=c+j|0;a=d+(j>>>0<q>>>0?a+1|0:a)|0;a=b>>>0<c>>>0?a+1|0:a;f[m+36>>2]=b&33554431;c=z&67108863;b=kc((a&33554431)<<7|b>>>25,0,19)+c|0;a=B;a=b>>>0<c>>>0?a+1|0:a;f[m>>2]=b&67108863;f[m+4>>2]=(C&33554431)+((a&67108863)<<6|b>>>26)}function qa(a,b){var c=0,e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0;c=A-272|0;A=c;i=g[b+31|0];u=a+40|0;ga(u,b);f[a+80>>2]=1;b=0;j=Q(a+84|0,0,36);Y(c+96|0,u);N(c+48|0,c+96|0,25648);h=f[a+80>>2];k=(f[c+96>>2]-h|0)+268435380|0;j=f[j>>2];e=((f[c+100>>2]+(k>>>26|0)|0)-j|0)+134217724|0;f[c+100>>2]=e&33554431;m=f[a+88>>2];e=(f[c+104>>2]-m+(e>>>25)|0)+268435452|0;f[c+104>>2]=e&67108863;n=f[a+92>>2];e=(f[c+108>>2]-n+(e>>>26)|0)+134217724|0;f[c+108>>2]=e&33554431;o=f[a+96>>2];e=(f[c+112>>2]-o+(e>>>25)|0)+268435452|0;f[c+112>>2]=e&67108863;p=f[a+100>>2];e=(f[c+116>>2]-p+(e>>>26)|0)+134217724|0;f[c+116>>2]=e&33554431;q=f[a+104>>2];e=(f[c+120>>2]-q+(e>>>25)|0)+268435452|0;f[c+120>>2]=e&67108863;r=f[a+108>>2];e=(f[c+124>>2]-r+(e>>>26)|0)+134217724|0;f[c+124>>2]=e&33554431;s=f[a+112>>2];e=(f[c+128>>2]-s+(e>>>25)|0)+268435452|0;f[c+128>>2]=e&67108863;t=f[a+116>>2];e=(f[c+132>>2]-t+(e>>>26)|0)+134217724|0;f[c+132>>2]=e&33554431;k=l(e>>>25|0,19)+(k&67108863)|0;f[c+96>>2]=k;f[c+48>>2]=h+f[c+48>>2];f[c+52>>2]=j+f[c+52>>2];f[c+56>>2]=m+f[c+56>>2];f[c+60>>2]=n+f[c+60>>2];f[c+64>>2]=o+f[c+64>>2];f[c+68>>2]=p+f[c+68>>2];f[c+72>>2]=q+f[c+72>>2];f[c+76>>2]=r+f[c+76>>2];f[c+80>>2]=s+f[c+80>>2];f[c+84>>2]=t+f[c+84>>2];Y(c+192|0,c+48|0);N(c,c+192|0,c+48|0);Y(a,c);N(a,a,c+48|0);N(a,a,c+96|0);ob(a,a);N(a,a,c);N(a,a,c+96|0);Y(c+192|0,a);N(c+192|0,c+192|0,c+48|0);h=f[c+196>>2];j=(f[c+192>>2]-k|0)+268435380|0;k=f[c+100>>2];e=((h+(j>>>26|0)|0)-k|0)+134217724|0;f[c+148>>2]=e&33554431;m=f[c+200>>2];n=f[c+104>>2];e=(m-n+(e>>>25)|0)+268435452|0;f[c+152>>2]=e&67108863;o=f[c+204>>2];p=f[c+108>>2];e=(o-p+(e>>>26)|0)+134217724|0;f[c+156>>2]=e&33554431;q=f[c+208>>2];r=f[c+112>>2];e=(q-r+(e>>>25)|0)+268435452|0;f[c+160>>2]=e&67108863;s=f[c+212>>2];t=f[c+116>>2];e=(s-t+(e>>>26)|0)+134217724|0;f[c+164>>2]=e&33554431;v=f[c+216>>2];w=f[c+120>>2];e=(v-w+(e>>>25)|0)+268435452|0;f[c+168>>2]=e&67108863;x=f[c+220>>2];y=f[c+124>>2];e=(x-y+(e>>>26)|0)+134217724|0;f[c+172>>2]=e&33554431;z=f[c+224>>2];B=f[c+128>>2];e=(z-B+(e>>>25)|0)+268435452|0;f[c+176>>2]=e&67108863;C=f[c+228>>2];D=f[c+132>>2];e=(C-D+(e>>>26)|0)+134217724|0;f[c+180>>2]=e&33554431;f[c+144>>2]=l(e>>>25|0,19)+(j&67108863);ca(c+240|0,c+144|0);a:{if(!pa(c+240|0,25696)){j=f[c+96>>2]+f[c+192>>2]|0;h=k+((j>>>26|0)+h|0)|0;f[c+196>>2]=h&33554431;h=n+(m+(h>>>25|0)|0)|0;f[c+200>>2]=h&67108863;h=o+p+(h>>>26)|0;f[c+204>>2]=h&33554431;h=q+r+(h>>>25)|0;f[c+208>>2]=h&67108863;h=s+t+(h>>>26)|0;f[c+212>>2]=h&33554431;h=v+w+(h>>>25)|0;f[c+216>>2]=h&67108863;h=x+y+(h>>>26)|0;f[c+220>>2]=h&33554431;h=z+B+(h>>>25)|0;f[c+224>>2]=h&67108863;h=C+D+(h>>>26)|0;f[c+228>>2]=h&33554431;f[c+192>>2]=l(h>>>25|0,19)+(j&67108863);ca(c+240|0,c+192|0);if(!pa(c+240|0,25696)){break a}N(a,a,25728)}ca(c+240|0,a);if((d[c+240|0]&1)==(i>>>7|0)){b=f[a>>2];f[c+192>>2]=b;i=f[a+4>>2];f[c+196>>2]=i;j=f[a+8>>2];f[c+200>>2]=j;h=f[a+12>>2];f[c+204>>2]=h;k=f[a+16>>2];f[c+208>>2]=k;e=f[a+20>>2];f[c+212>>2]=e;m=f[a+24>>2];f[c+216>>2]=m;n=f[a+28>>2];f[c+220>>2]=n;o=f[a+32>>2];f[c+224>>2]=o;p=f[a+36>>2];f[c+228>>2]=p;b=134217690-b|0;i=((b>>>26|0)-i|0)+67108862|0;j=((i>>>25|0)-j|0)+134217726|0;h=((j>>>26|0)-h|0)+67108862|0;k=((h>>>25|0)-k|0)+134217726|0;e=((k>>>26|0)-e|0)+67108862|0;f[a+20>>2]=e&33554431;f[a+16>>2]=k&67108863;f[a+12>>2]=h&33554431;f[a+8>>2]=j&67108863;f[a+4>>2]=i&33554431;i=((e>>>25|0)-m|0)+134217726|0;f[a+24>>2]=i&67108863;i=((i>>>26|0)-n|0)+67108862|0;f[a+28>>2]=i&33554431;i=((i>>>25|0)-o|0)+134217726|0;f[a+32>>2]=i&67108863;i=((i>>>26|0)-p|0)+67108862|0;f[a+36>>2]=i&33554431;f[a>>2]=l(i>>>25|0,19)+(b&67108863)}N(a+120|0,a,u);b=1}A=c+272|0;return b}function lb(a,b,c,d){var e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0;e=A-144|0;A=e;h=f[b+40>>2];i=f[b>>2];g=(h-i|0)+134217690|0;f[e+96>>2]=g&67108863;j=f[b+44>>2];m=f[b+4>>2];g=((j+(g>>>26|0)|0)-m|0)+67108862|0;f[e+100>>2]=g&33554431;n=f[b+48>>2];o=f[b+8>>2];g=(n-o+(g>>>25)|0)+134217726|0;f[e+104>>2]=g&67108863;p=f[b+52>>2];q=f[b+12>>2];g=(p-q+(g>>>26)|0)+67108862|0;f[e+108>>2]=g&33554431;r=f[b+56>>2];s=f[b+16>>2];f[e+112>>2]=(r-s+(g>>>25)|0)+134217726;t=f[b+60>>2];u=f[b+20>>2];f[e+116>>2]=(t-u|0)+67108862;v=f[b- -64>>2];w=f[b+24>>2];f[e+120>>2]=(v-w|0)+134217726;x=f[b+68>>2];y=f[b+28>>2];f[e+124>>2]=(x-y|0)+67108862;z=f[b+72>>2];B=f[b+32>>2];f[e+128>>2]=(z-B|0)+134217726;g=f[b+76>>2];k=f[b+36>>2];f[e+80>>2]=z+B;f[e+76>>2]=x+y;f[e+72>>2]=v+w;f[e+68>>2]=t+u;f[e+64>>2]=r+s;f[e+60>>2]=p+q;f[e+56>>2]=n+o;f[e+52>>2]=j+m;f[e+48>>2]=h+i;f[e+84>>2]=g+k;f[e+132>>2]=(g-k|0)+67108862;g=l(d,40);N(e+96|0,e+96|0,g+c|0);d=l(d^1,40);N(a,e+48|0,d+c|0);k=f[e+96>>2];h=f[a>>2];f[a+40>>2]=k+h;i=f[e+100>>2];j=f[a+4>>2];f[a+44>>2]=i+j;m=f[e+104>>2];n=f[a+8>>2];f[a+48>>2]=m+n;o=f[e+108>>2];p=f[a+12>>2];f[a+52>>2]=o+p;q=f[e+112>>2];r=f[a+16>>2];f[a+56>>2]=q+r;s=f[e+116>>2];t=f[a+20>>2];f[a+60>>2]=s+t;u=f[e+120>>2];v=f[a+24>>2];f[a- -64>>2]=u+v;w=f[e+124>>2];x=f[a+28>>2];f[a+68>>2]=w+x;y=f[e+128>>2];z=f[a+32>>2];f[a+72>>2]=y+z;B=f[a+36>>2];C=f[e+132>>2];f[a+76>>2]=B+C;f[a+28>>2]=(x-w|0)+67108862;f[a+24>>2]=(v-u|0)+134217726;f[a+32>>2]=(z-y|0)+134217726;k=(h-k|0)+134217690|0;h=((j+(k>>>26|0)|0)-i|0)+67108862|0;i=(n-m+(h>>>25)|0)+134217726|0;j=(p-o+(i>>>26)|0)+67108862|0;f[a+16>>2]=(r-q+(j>>>25)|0)+134217726;f[a+20>>2]=(t-s|0)+67108862;f[a+36>>2]=(B-C|0)+67108862;f[a+12>>2]=j&33554431;f[a+8>>2]=i&67108863;f[a+4>>2]=h&33554431;f[a>>2]=k&67108863;N(e,b+120|0,c+120|0);N(a+120|0,b+80|0,c+80|0);b=f[a+120>>2];c=(b>>>25&63)+(f[a+124>>2]<<1)|0;k=c&33554431;f[a+124>>2]=k;c=(f[a+128>>2]<<1)+(c>>>25|0)|0;h=c&67108863;f[a+128>>2]=h;c=(f[a+132>>2]<<1)+(c>>>26|0)|0;i=c&33554431;f[a+132>>2]=i;c=(f[a+136>>2]<<1)+(c>>>25|0)|0;j=c&67108863;f[a+136>>2]=j;c=(f[a+140>>2]<<1)+(c>>>26|0)|0;m=c&33554431;f[a+140>>2]=m;c=(f[a+144>>2]<<1)+(c>>>25|0)|0;n=c&67108863;f[a+144>>2]=n;c=(f[a+148>>2]<<1)+(c>>>26|0)|0;o=c&33554431;f[a+148>>2]=o;c=(f[a+152>>2]<<1)+(c>>>25|0)|0;p=c&67108863;f[a+152>>2]=p;c=(f[a+156>>2]<<1)+(c>>>26|0)|0;q=c&33554431;f[a+156>>2]=q;b=l(c>>>25|0,19)+(b<<1&67108862)|0;f[a+120>>2]=b;f[a+116>>2]=q;f[a+112>>2]=p;f[a+108>>2]=o;f[a+104>>2]=n;f[a+100>>2]=m;f[a+96>>2]=j;f[a+92>>2]=i;f[a+88>>2]=h;f[a+84>>2]=k;f[a+80>>2]=b;b=a+g|0;c=f[e>>2];f[b+80>>2]=c+f[b+80>>2];g=f[e+4>>2];f[b+84>>2]=g+f[b+84>>2];k=f[e+8>>2];f[b+88>>2]=k+f[b+88>>2];h=f[e+12>>2];f[b+92>>2]=h+f[b+92>>2];i=f[e+16>>2];f[b+96>>2]=i+f[b+96>>2];j=f[e+20>>2];f[b+100>>2]=j+f[b+100>>2];m=f[e+24>>2];f[b+104>>2]=m+f[b+104>>2];n=f[e+28>>2];f[b+108>>2]=n+f[b+108>>2];o=f[e+32>>2];f[b+112>>2]=o+f[b+112>>2];p=b;q=f[b+116>>2];b=f[e+36>>2];f[p+116>>2]=q+b;a=a+d|0;f[a+100>>2]=(f[a+100>>2]-j|0)+67108862;f[a+104>>2]=(f[a+104>>2]-m|0)+134217726;f[a+108>>2]=(f[a+108>>2]-n|0)+67108862;f[a+112>>2]=(f[a+112>>2]-o|0)+134217726;f[a+116>>2]=(f[a+116>>2]-b|0)+67108862;b=(f[a+80>>2]-c|0)+134217690|0;f[a+80>>2]=b&67108863;b=(f[a+84>>2]-g+(b>>>26)|0)+67108862|0;f[a+84>>2]=b&33554431;b=(f[a+88>>2]-k+(b>>>25)|0)+134217726|0;f[a+88>>2]=b&67108863;b=(f[a+92>>2]-h+(b>>>26)|0)+67108862|0;f[a+92>>2]=b&33554431;f[a+96>>2]=(f[a+96>>2]-i+(b>>>25)|0)+134217726;A=e+144|0}function Ta(a,b,c,e){var h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,Q=0,R=0,S=0,T=0,U=0,V=0,W=0,X=0,Y=0,Z=0,_=0,$=0,aa=0,ba=0,ca=0,da=0;H=A+ -64|0;A=H;if(e){I=f[a+60>>2];J=f[a+56>>2];z=f[a+52>>2];B=f[a+48>>2];K=f[a+44>>2];L=f[a+40>>2];M=f[a+36>>2];N=f[a+32>>2];O=f[a+28>>2];Q=f[a+24>>2];R=f[a+20>>2];S=f[a+16>>2];T=f[a+12>>2];U=f[a+8>>2];V=f[a+4>>2];W=f[a>>2];while(1){a:{if(e>>>0>63){h=c;break a}h=P(H,b,e);b=h;X=c}C=20;j=W;m=V;n=U;q=T;c=S;l=R;r=Q;s=O;k=N;w=M;o=L;i=I;x=J;t=z;p=B;u=K;while(1){v=c;c=c+j|0;j=lc(c^p,16);k=j+k|0;p=lc(v^k,12);v=p;y=c+p|0;p=lc(y^j,8);k=p+k|0;c=lc(v^k,7);j=s+q|0;i=lc(j^i,16);u=i+u|0;s=lc(u^s,12);n=r+n|0;q=lc(n^x,16);o=q+o|0;r=lc(o^r,12);D=j+s|0;x=D+c|0;n=r+n|0;E=lc(n^q,8);j=lc(x^E,16);m=l+m|0;q=lc(m^t,16);w=q+w|0;l=lc(w^l,12);v=c;m=l+m|0;t=lc(m^q,8);F=t+w|0;c=F+j|0;G=lc(v^c,12);q=G+x|0;x=lc(j^q,8);w=x+c|0;c=lc(w^G,7);v=k;k=n;j=lc(i^D,8);i=j+u|0;n=lc(i^s,7);k=k+n|0;t=lc(k^t,16);s=v+t|0;u=lc(s^n,12);n=u+k|0;t=lc(t^n,8);k=s+t|0;s=lc(k^u,7);v=i;i=m;o=o+E|0;m=lc(o^r,7);i=i+m|0;p=lc(i^p,16);r=v+p|0;v=i;i=lc(r^m,12);m=v+i|0;p=lc(p^m,8);u=r+p|0;r=lc(u^i,7);v=o;i=j;j=lc(l^F,7);o=j+y|0;i=lc(i^o,16);l=v+i|0;y=lc(j^l,12);j=y+o|0;i=lc(i^j,8);o=l+i|0;l=lc(o^y,7);C=C+ -2|0;if(C){continue}break}C=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);y=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);D=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);E=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);F=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);G=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);Y=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);Z=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);_=g[b+32|0]|g[b+33|0]<<8|(g[b+34|0]<<16|g[b+35|0]<<24);$=g[b+36|0]|g[b+37|0]<<8|(g[b+38|0]<<16|g[b+39|0]<<24);aa=g[b+40|0]|g[b+41|0]<<8|(g[b+42|0]<<16|g[b+43|0]<<24);ba=g[b+44|0]|g[b+45|0]<<8|(g[b+46|0]<<16|g[b+47|0]<<24);ca=g[b+48|0]|g[b+49|0]<<8|(g[b+50|0]<<16|g[b+51|0]<<24);da=g[b+52|0]|g[b+53|0]<<8|(g[b+54|0]<<16|g[b+55|0]<<24);v=g[b+56|0]|g[b+57|0]<<8|(g[b+58|0]<<16|g[b+59|0]<<24);i=i+I^(g[b+60|0]|g[b+61|0]<<8|(g[b+62|0]<<16|g[b+63|0]<<24));d[h+60|0]=i;x=x+J^v;d[h+56|0]=x;t=t+z^da;d[h+52|0]=t;p=p+B^ca;d[h+48|0]=p;u=u+K^ba;d[h+44|0]=u;o=o+L^aa;d[h+40|0]=o;w=w+M^$;d[h+36|0]=w;k=k+N^_;d[h+32|0]=k;s=s+O^Z;d[h+28|0]=s;r=r+Q^Y;d[h+24|0]=r;l=G^l+R;d[h+20|0]=l;c=F^c+S;d[h+16|0]=c;q=E^q+T;d[h+12|0]=q;n=D^n+U;d[h+8|0]=n;m=y^m+V;d[h+4|0]=m;j=C^j+W;d[h|0]=j;d[h+63|0]=i>>>24;d[h+62|0]=i>>>16;d[h+61|0]=i>>>8;d[h+59|0]=x>>>24;d[h+58|0]=x>>>16;d[h+57|0]=x>>>8;d[h+55|0]=t>>>24;d[h+54|0]=t>>>16;d[h+53|0]=t>>>8;d[h+51|0]=p>>>24;d[h+50|0]=p>>>16;d[h+49|0]=p>>>8;d[h+47|0]=u>>>24;d[h+46|0]=u>>>16;d[h+45|0]=u>>>8;d[h+43|0]=o>>>24;d[h+42|0]=o>>>16;d[h+41|0]=o>>>8;d[h+39|0]=w>>>24;d[h+38|0]=w>>>16;d[h+37|0]=w>>>8;d[h+35|0]=k>>>24;d[h+34|0]=k>>>16;d[h+33|0]=k>>>8;d[h+31|0]=s>>>24;d[h+30|0]=s>>>16;d[h+29|0]=s>>>8;d[h+27|0]=r>>>24;d[h+26|0]=r>>>16;d[h+25|0]=r>>>8;d[h+23|0]=l>>>24;d[h+22|0]=l>>>16;d[h+21|0]=l>>>8;d[h+19|0]=c>>>24;d[h+18|0]=c>>>16;d[h+17|0]=c>>>8;d[h+15|0]=q>>>24;d[h+14|0]=q>>>16;d[h+13|0]=q>>>8;d[h+11|0]=n>>>24;d[h+10|0]=n>>>16;d[h+9|0]=n>>>8;d[h+7|0]=m>>>24;d[h+6|0]=m>>>16;d[h+5|0]=m>>>8;d[h+3|0]=j>>>24;d[h+2|0]=j>>>16;d[h+1|0]=j>>>8;l=B+1|0;z=(l>>>0<B>>>0)+z|0;if(e>>>0<=64){if(e>>>0<=63){P(X,h,e)}f[a+52>>2]=z;f[a+48>>2]=l}else{b=b- -64|0;c=h- -64|0;e=e+ -64|0;B=l;continue}break}}A=H- -64|0}function kb(a,b,c,d){var e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0;e=A-144|0;A=e;h=f[b+40>>2];i=f[b>>2];g=(h-i|0)+134217690|0;f[e+96>>2]=g&67108863;j=f[b+44>>2];m=f[b+4>>2];g=((j+(g>>>26|0)|0)-m|0)+67108862|0;f[e+100>>2]=g&33554431;n=f[b+48>>2];o=f[b+8>>2];g=(n-o+(g>>>25)|0)+134217726|0;f[e+104>>2]=g&67108863;p=f[b+52>>2];q=f[b+12>>2];g=(p-q+(g>>>26)|0)+67108862|0;f[e+108>>2]=g&33554431;r=f[b+56>>2];s=f[b+16>>2];f[e+112>>2]=(r-s+(g>>>25)|0)+134217726;t=f[b+60>>2];u=f[b+20>>2];f[e+116>>2]=(t-u|0)+67108862;v=f[b- -64>>2];w=f[b+24>>2];f[e+120>>2]=(v-w|0)+134217726;x=f[b+68>>2];y=f[b+28>>2];f[e+124>>2]=(x-y|0)+67108862;z=f[b+72>>2];B=f[b+32>>2];f[e+128>>2]=(z-B|0)+134217726;g=f[b+76>>2];k=f[b+36>>2];f[e+80>>2]=z+B;f[e+76>>2]=x+y;f[e+72>>2]=v+w;f[e+68>>2]=t+u;f[e+64>>2]=r+s;f[e+60>>2]=p+q;f[e+56>>2]=n+o;f[e+52>>2]=j+m;f[e+48>>2]=h+i;f[e+84>>2]=g+k;f[e+132>>2]=(g-k|0)+67108862;g=l(d,40);N(e+96|0,e+96|0,g+c|0);d=l(d^1,40);N(a,e+48|0,d+c|0);k=f[e+96>>2];h=f[a>>2];f[a+40>>2]=k+h;i=f[e+100>>2];j=f[a+4>>2];f[a+44>>2]=i+j;m=f[e+104>>2];n=f[a+8>>2];f[a+48>>2]=m+n;o=f[e+108>>2];p=f[a+12>>2];f[a+52>>2]=o+p;q=f[e+112>>2];r=f[a+16>>2];f[a+56>>2]=q+r;s=f[e+116>>2];t=f[a+20>>2];f[a+60>>2]=s+t;u=f[e+120>>2];v=f[a+24>>2];f[a- -64>>2]=u+v;w=f[e+124>>2];x=f[a+28>>2];f[a+68>>2]=w+x;y=f[e+128>>2];z=f[a+32>>2];f[a+72>>2]=y+z;B=f[a+36>>2];C=f[e+132>>2];f[a+76>>2]=B+C;f[a+28>>2]=(x-w|0)+67108862;f[a+24>>2]=(v-u|0)+134217726;f[a+32>>2]=(z-y|0)+134217726;k=(h-k|0)+134217690|0;h=((j+(k>>>26|0)|0)-i|0)+67108862|0;i=(n-m+(h>>>25)|0)+134217726|0;j=(p-o+(i>>>26)|0)+67108862|0;f[a+16>>2]=(r-q+(j>>>25)|0)+134217726;f[a+20>>2]=(t-s|0)+67108862;f[a+36>>2]=(B-C|0)+67108862;f[a+12>>2]=j&33554431;f[a+8>>2]=i&67108863;f[a+4>>2]=h&33554431;f[a>>2]=k&67108863;N(e,b+120|0,c+80|0);c=f[b+80>>2];k=c<<1&67108862;f[a+120>>2]=k;c=(c>>>25&63)+(f[b+84>>2]<<1)|0;h=c&33554431;f[a+124>>2]=h;c=(f[b+88>>2]<<1)+(c>>>25|0)|0;i=c&67108863;f[a+128>>2]=i;c=(f[b+92>>2]<<1)+(c>>>26|0)|0;j=c&33554431;f[a+132>>2]=j;c=(f[b+96>>2]<<1)+(c>>>25|0)|0;m=c&67108863;f[a+136>>2]=m;c=(f[b+100>>2]<<1)+(c>>>26|0)|0;n=c&33554431;f[a+140>>2]=n;c=(f[b+104>>2]<<1)+(c>>>25|0)|0;o=c&67108863;f[a+144>>2]=o;c=(f[b+108>>2]<<1)+(c>>>26|0)|0;p=c&33554431;f[a+148>>2]=p;c=(f[b+112>>2]<<1)+(c>>>25|0)|0;q=c&67108863;f[a+152>>2]=q;b=f[b+116>>2];f[a+112>>2]=q;f[a+108>>2]=p;f[a+104>>2]=o;f[a+100>>2]=n;f[a+96>>2]=m;f[a+92>>2]=j;f[a+88>>2]=i;f[a+84>>2]=h;b=(b<<1)+(c>>>26|0)|0;c=b&33554431;f[a+156>>2]=c;f[a+116>>2]=c;b=k+l(b>>>25|0,19)|0;f[a+120>>2]=b;f[a+80>>2]=b;b=a+g|0;c=f[e>>2];f[b+80>>2]=c+f[b+80>>2];g=f[e+4>>2];f[b+84>>2]=g+f[b+84>>2];k=f[e+8>>2];f[b+88>>2]=k+f[b+88>>2];h=f[e+12>>2];f[b+92>>2]=h+f[b+92>>2];i=f[e+16>>2];f[b+96>>2]=i+f[b+96>>2];j=f[e+20>>2];f[b+100>>2]=j+f[b+100>>2];m=f[e+24>>2];f[b+104>>2]=m+f[b+104>>2];n=f[e+28>>2];f[b+108>>2]=n+f[b+108>>2];o=f[e+32>>2];f[b+112>>2]=o+f[b+112>>2];p=b;q=f[b+116>>2];b=f[e+36>>2];f[p+116>>2]=q+b;a=a+d|0;f[a+100>>2]=(f[a+100>>2]-j|0)+67108862;f[a+104>>2]=(f[a+104>>2]-m|0)+134217726;f[a+108>>2]=(f[a+108>>2]-n|0)+67108862;f[a+112>>2]=(f[a+112>>2]-o|0)+134217726;f[a+116>>2]=(f[a+116>>2]-b|0)+67108862;b=(f[a+80>>2]-c|0)+134217690|0;f[a+80>>2]=b&67108863;b=(f[a+84>>2]-g+(b>>>26)|0)+67108862|0;f[a+84>>2]=b&33554431;b=(f[a+88>>2]-k+(b>>>25)|0)+134217726|0;f[a+88>>2]=b&67108863;b=(f[a+92>>2]-h+(b>>>26)|0)+67108862|0;f[a+92>>2]=b&33554431;f[a+96>>2]=(f[a+96>>2]-i+(b>>>25)|0)+134217726;A=e+144|0}function ua(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0;c=A-336|0;A=c;h=f[a+40>>2];i=f[a>>2];d=(h-i|0)+134217690|0;f[c+288>>2]=d&67108863;j=f[a+44>>2];k=f[a+4>>2];d=((j+(d>>>26|0)|0)-k|0)+67108862|0;f[c+292>>2]=d&33554431;m=f[a+48>>2];n=f[a+8>>2];d=(m-n+(d>>>25)|0)+134217726|0;f[c+296>>2]=d&67108863;o=f[a+52>>2];r=f[a+12>>2];d=(o-r+(d>>>26)|0)+67108862|0;f[c+300>>2]=d&33554431;p=f[a+56>>2];q=f[a+16>>2];f[c+304>>2]=(p-q+(d>>>25)|0)+134217726;s=f[a+60>>2];e=f[a+20>>2];f[c+308>>2]=(s-e|0)+67108862;t=f[a- -64>>2];u=f[a+24>>2];f[c+312>>2]=(t-u|0)+134217726;v=f[a+68>>2];w=f[a+28>>2];f[c+316>>2]=(v-w|0)+67108862;x=f[a+72>>2];y=f[a+32>>2];f[c+320>>2]=(x-y|0)+134217726;d=f[a+76>>2];g=f[a+36>>2];f[c+272>>2]=x+y;f[c+268>>2]=v+w;f[c+264>>2]=t+u;f[c+260>>2]=e+s;f[c+256>>2]=p+q;f[c+252>>2]=o+r;f[c+248>>2]=m+n;f[c+244>>2]=j+k;f[c+240>>2]=h+i;f[c+276>>2]=d+g;f[c+324>>2]=(d-g|0)+67108862;N(c+288|0,c+288|0,b);N(c+144|0,c+240|0,b+40|0);d=f[c+288>>2];g=f[c+144>>2];f[c>>2]=d+g;h=f[c+292>>2];i=f[c+148>>2];f[c+4>>2]=h+i;j=f[c+296>>2];k=f[c+152>>2];f[c+8>>2]=j+k;m=f[c+300>>2];n=f[c+156>>2];f[c+12>>2]=m+n;o=f[c+304>>2];r=f[c+160>>2];f[c+16>>2]=o+r;p=f[c+308>>2];q=f[c+164>>2];f[c+20>>2]=p+q;f[c+164>>2]=(q-p|0)+67108862;d=(g-d|0)+134217690|0;f[c+144>>2]=d&67108863;g=f[c+312>>2];p=f[c+168>>2];f[c+24>>2]=g+p;q=f[c+316>>2];s=f[c+172>>2];f[c+28>>2]=q+s;e=f[c+320>>2];t=f[c+176>>2];f[c+32>>2]=e+t;u=f[c+324>>2];v=f[c+180>>2];f[c+36>>2]=u+v;f[c+172>>2]=(s-q|0)+67108862;f[c+176>>2]=(t-e|0)+134217726;f[c+180>>2]=(v-u|0)+67108862;f[c+168>>2]=(p-g|0)+134217726;d=((i+(d>>>26|0)|0)-h|0)+67108862|0;f[c+148>>2]=d&33554431;d=(k-j+(d>>>25)|0)+134217726|0;f[c+152>>2]=d&67108863;d=(n-m+(d>>>26)|0)+67108862|0;f[c+156>>2]=d&33554431;f[c+160>>2]=(r-o+(d>>>25)|0)+134217726;r=a+120|0;N(c+192|0,r,b+80|0);b=f[a+80>>2]<<1;f[c+96>>2]=b;d=f[a+84>>2]<<1;f[c+100>>2]=d;g=f[a+88>>2]<<1;f[c+104>>2]=g;h=f[a+92>>2]<<1;f[c+108>>2]=h;i=f[a+96>>2]<<1;f[c+112>>2]=i;j=f[a+100>>2]<<1;f[c+116>>2]=j;k=f[a+104>>2]<<1;f[c+120>>2]=k;m=f[a+108>>2]<<1;f[c+124>>2]=m;n=f[a+112>>2]<<1;f[c+128>>2]=n;o=f[a+116>>2]<<1;f[c+132>>2]=o;s=f[c+196>>2];p=f[c+192>>2];q=p+b|0;e=s+(d+(q>>>26|0)|0)|0;f[c+52>>2]=e&33554431;t=f[c+200>>2];e=t+g+(e>>>25)|0;f[c+56>>2]=e&67108863;u=f[c+204>>2];e=u+h+(e>>>26)|0;f[c+60>>2]=e&33554431;v=f[c+208>>2];e=v+i+(e>>>25)|0;f[c+64>>2]=e&67108863;w=f[c+212>>2];e=w+j+(e>>>26)|0;f[c+68>>2]=e&33554431;x=f[c+216>>2];e=x+k+(e>>>25)|0;f[c+72>>2]=e&67108863;y=f[c+220>>2];e=y+m+(e>>>26)|0;f[c+76>>2]=e&33554431;z=f[c+224>>2];e=n+z+(e>>>25)|0;f[c+80>>2]=e&67108863;B=f[c+228>>2];e=o+B+(e>>>26)|0;f[c+84>>2]=e&33554431;f[c+48>>2]=l(e>>>25|0,19)+(q&67108863);b=(b-p|0)+268435380|0;d=((d+(b>>>26|0)|0)-s|0)+134217724|0;g=(g-t+(d>>>25)|0)+268435452|0;h=(h-u+(g>>>26)|0)+134217724|0;i=(i-v+(h>>>25)|0)+268435452|0;j=(j-w+(i>>>26)|0)+134217724|0;k=(k-x+(j>>>25)|0)+268435452|0;m=(m-y+(k>>>26)|0)+134217724|0;n=(n-z+(m>>>25)|0)+268435452|0;o=(o-B+(n>>>26)|0)+134217724|0;f[c+132>>2]=o&33554431;f[c+128>>2]=n&67108863;f[c+124>>2]=m&33554431;f[c+120>>2]=k&67108863;f[c+116>>2]=j&33554431;f[c+112>>2]=i&67108863;f[c+108>>2]=h&33554431;f[c+104>>2]=g&67108863;f[c+100>>2]=d&33554431;f[c+96>>2]=l(o>>>25|0,19)+(b&67108863);N(a,c+144|0,c+96|0);N(a+40|0,c,c+48|0);N(a+80|0,c+48|0,c+96|0);N(r,c+144|0,c);A=c+336|0}function na(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0;d=A-144|0;A=d;Y(d+96|0,b);Y(d+48|0,b+40|0);Y(d,b+80|0);g=f[d>>2];c=(g>>>25&63)+(f[d+4>>2]<<1)|0;f[d+4>>2]=c&33554431;c=(f[d+8>>2]<<1)+(c>>>25|0)|0;f[d+8>>2]=c&67108863;c=(f[d+12>>2]<<1)+(c>>>26|0)|0;f[d+12>>2]=c&33554431;c=(f[d+16>>2]<<1)+(c>>>25|0)|0;f[d+16>>2]=c&67108863;c=(f[d+20>>2]<<1)+(c>>>26|0)|0;f[d+20>>2]=c&33554431;c=(f[d+24>>2]<<1)+(c>>>25|0)|0;f[d+24>>2]=c&67108863;c=(f[d+28>>2]<<1)+(c>>>26|0)|0;f[d+28>>2]=c&33554431;c=(f[d+32>>2]<<1)+(c>>>25|0)|0;f[d+32>>2]=c&67108863;c=(f[d+36>>2]<<1)+(c>>>26|0)|0;f[d+36>>2]=c&33554431;f[d>>2]=l(c>>>25|0,19)+(g<<1&67108862);f[a>>2]=f[b+40>>2]+f[b>>2];f[a+4>>2]=f[b+44>>2]+f[b+4>>2];f[a+8>>2]=f[b+48>>2]+f[b+8>>2];f[a+12>>2]=f[b+52>>2]+f[b+12>>2];f[a+16>>2]=f[b+56>>2]+f[b+16>>2];f[a+20>>2]=f[b+60>>2]+f[b+20>>2];f[a+24>>2]=f[b- -64>>2]+f[b+24>>2];f[a+28>>2]=f[b+68>>2]+f[b+28>>2];f[a+32>>2]=f[b+72>>2]+f[b+32>>2];f[a+36>>2]=f[b+76>>2]+f[b+36>>2];Y(a,a);c=f[d+96>>2];h=f[d+48>>2];e=c+h|0;f[a+40>>2]=e;i=f[d+100>>2];j=f[d+52>>2];r=i+j|0;f[a+44>>2]=r;k=f[d+104>>2];m=f[d+56>>2];s=k+m|0;f[a+48>>2]=s;t=f[d+108>>2];u=f[d+60>>2];v=t+u|0;f[a+52>>2]=v;w=f[d+112>>2];x=f[d+64>>2];y=w+x|0;f[a+56>>2]=y;n=f[d+68>>2];z=f[d+116>>2];B=n+z|0;f[a+60>>2]=B;o=f[d+72>>2];C=f[d+120>>2];D=o+C|0;f[a- -64>>2]=D;p=f[d+76>>2];E=f[d+124>>2];F=p+E|0;f[a+68>>2]=F;q=f[d+80>>2];G=f[d+128>>2];H=q+G|0;f[a+72>>2]=H;b=f[d+132>>2];g=f[d+84>>2];q=(q-G|0)+134217726|0;f[a+112>>2]=q;p=(p-E|0)+67108862|0;f[a+108>>2]=p;o=(o-C|0)+134217726|0;f[a+104>>2]=o;n=(n-z|0)+67108862|0;f[a+100>>2]=n;c=(h-c|0)+134217690|0;h=((j+(c>>>26|0)|0)-i|0)+67108862|0;i=(m-k+(h>>>25)|0)+134217726|0;j=(u-t+(i>>>26)|0)+67108862|0;k=(x-w+(j>>>25)|0)+134217726|0;f[a+96>>2]=k;j=j&33554431;f[a+92>>2]=j;i=i&67108863;f[a+88>>2]=i;h=h&33554431;f[a+84>>2]=h;c=c&67108863;f[a+80>>2]=c;m=b+g|0;f[a+76>>2]=m;b=(g-b|0)+67108862|0;f[a+116>>2]=b;g=(f[a>>2]-e|0)+268435380|0;e=(f[a+4>>2]-r+(g>>>26)|0)+134217724|0;f[a+4>>2]=e&33554431;e=(f[a+8>>2]-s+(e>>>25)|0)+268435452|0;f[a+8>>2]=e&67108863;e=(f[a+12>>2]-v+(e>>>26)|0)+134217724|0;f[a+12>>2]=e&33554431;e=(f[a+16>>2]-y+(e>>>25)|0)+268435452|0;f[a+16>>2]=e&67108863;e=(f[a+20>>2]-B+(e>>>26)|0)+134217724|0;f[a+20>>2]=e&33554431;e=(f[a+24>>2]-D+(e>>>25)|0)+268435452|0;f[a+24>>2]=e&67108863;e=(f[a+28>>2]-F+(e>>>26)|0)+134217724|0;f[a+28>>2]=e&33554431;e=(f[a+32>>2]-H+(e>>>25)|0)+268435452|0;f[a+32>>2]=e&67108863;e=(f[a+36>>2]-m+(e>>>26)|0)+134217724|0;f[a+36>>2]=e&33554431;f[a>>2]=l(e>>>25|0,19)+(g&67108863);g=(f[d>>2]-c|0)+268435380|0;c=(f[d+4>>2]-h+(g>>>26)|0)+134217724|0;f[a+124>>2]=c&33554431;c=(f[d+8>>2]-i+(c>>>25)|0)+268435452|0;f[a+128>>2]=c&67108863;c=(f[d+12>>2]-j+(c>>>26)|0)+134217724|0;f[a+132>>2]=c&33554431;c=(f[d+16>>2]-k+(c>>>25)|0)+268435452|0;f[a+136>>2]=c&67108863;c=(f[d+20>>2]-n+(c>>>26)|0)+134217724|0;f[a+140>>2]=c&33554431;c=(f[d+24>>2]-o+(c>>>25)|0)+268435452|0;f[a+144>>2]=c&67108863;c=(f[d+28>>2]-p+(c>>>26)|0)+134217724|0;f[a+148>>2]=c&33554431;c=(f[d+32>>2]-q+(c>>>25)|0)+268435452|0;f[a+152>>2]=c&67108863;b=(f[d+36>>2]-b+(c>>>26)|0)+134217724|0;f[a+156>>2]=b&33554431;f[a+120>>2]=l(b>>>25|0,19)+(g&67108863);A=d+144|0}function Tb(a){a=a|0;var b=0,c=0,d=0,e=0,g=0,h=0,j=0;a:{if(!a){break a}d=a+ -8|0;c=f[a+ -4>>2];a=c&-8;g=d+a|0;b:{if(c&1){break b}if(!(c&3)){break a}c=f[d>>2];d=d-c|0;if(d>>>0<i[7781]){break a}a=a+c|0;if(f[7782]!=(d|0)){if(c>>>0<=255){e=f[d+8>>2];c=c>>>3|0;b=f[d+12>>2];if((b|0)==(e|0)){f[7777]=f[7777]&lc(-2,c);break b}f[e+12>>2]=b;f[b+8>>2]=e;break b}j=f[d+24>>2];c=f[d+12>>2];c:{if((d|0)!=(c|0)){b=f[d+8>>2];f[b+12>>2]=c;f[c+8>>2]=b;break c}d:{e=d+20|0;b=f[e>>2];if(b){break d}e=d+16|0;b=f[e>>2];if(b){break d}c=0;break c}while(1){h=e;c=b;e=c+20|0;b=f[e>>2];if(b){continue}e=c+16|0;b=f[c+16>>2];if(b){continue}break}f[h>>2]=0}if(!j){break b}e=f[d+28>>2];b=(e<<2)+31412|0;e:{if(f[b>>2]==(d|0)){f[b>>2]=c;if(c){break e}f[7778]=f[7778]&lc(-2,e);break b}f[j+(f[j+16>>2]==(d|0)?16:20)>>2]=c;if(!c){break b}}f[c+24>>2]=j;b=f[d+16>>2];if(b){f[c+16>>2]=b;f[b+24>>2]=c}b=f[d+20>>2];if(!b){break b}f[c+20>>2]=b;f[b+24>>2]=c;break b}c=f[g+4>>2];if((c&3)!=3){break b}f[7779]=a;f[g+4>>2]=c&-2;f[d+4>>2]=a|1;f[a+d>>2]=a;return}if(g>>>0<=d>>>0){break a}c=f[g+4>>2];if(!(c&1)){break a}f:{if(!(c&2)){if(f[7783]==(g|0)){f[7783]=d;a=f[7780]+a|0;f[7780]=a;f[d+4>>2]=a|1;if(f[7782]!=(d|0)){break a}f[7779]=0;f[7782]=0;return}if(f[7782]==(g|0)){f[7782]=d;a=f[7779]+a|0;f[7779]=a;f[d+4>>2]=a|1;f[a+d>>2]=a;return}a=(c&-8)+a|0;g:{if(c>>>0<=255){b=f[g+8>>2];c=c>>>3|0;e=f[g+12>>2];if((b|0)==(e|0)){f[7777]=f[7777]&lc(-2,c);break g}f[b+12>>2]=e;f[e+8>>2]=b;break g}j=f[g+24>>2];c=f[g+12>>2];h:{if((g|0)!=(c|0)){b=f[g+8>>2];f[b+12>>2]=c;f[c+8>>2]=b;break h}i:{e=g+20|0;b=f[e>>2];if(b){break i}e=g+16|0;b=f[e>>2];if(b){break i}c=0;break h}while(1){h=e;c=b;e=c+20|0;b=f[e>>2];if(b){continue}e=c+16|0;b=f[c+16>>2];if(b){continue}break}f[h>>2]=0}if(!j){break g}e=f[g+28>>2];b=(e<<2)+31412|0;j:{if(f[b>>2]==(g|0)){f[b>>2]=c;if(c){break j}f[7778]=f[7778]&lc(-2,e);break g}f[j+(f[j+16>>2]==(g|0)?16:20)>>2]=c;if(!c){break g}}f[c+24>>2]=j;b=f[g+16>>2];if(b){f[c+16>>2]=b;f[b+24>>2]=c}b=f[g+20>>2];if(!b){break g}f[c+20>>2]=b;f[b+24>>2]=c}f[d+4>>2]=a|1;f[a+d>>2]=a;if(f[7782]!=(d|0)){break f}f[7779]=a;return}f[g+4>>2]=c&-2;f[d+4>>2]=a|1;f[a+d>>2]=a}if(a>>>0<=255){a=a>>>3|0;c=(a<<3)+31148|0;b=f[7777];a=1<<a;k:{if(!(b&a)){f[7777]=a|b;a=c;break k}a=f[c+8>>2]}f[c+8>>2]=d;f[a+12>>2]=d;f[d+12>>2]=c;f[d+8>>2]=a;return}f[d+16>>2]=0;f[d+20>>2]=0;g=d;e=a>>>8|0;b=0;l:{if(!e){break l}b=31;if(a>>>0>16777215){break l}c=e;e=e+1048320>>>16&8;b=c<<e;j=b+520192>>>16&4;b=b<<j;h=b+245760>>>16&2;b=(b<<h>>>15|0)-(h|(e|j))|0;b=(b<<1|a>>>b+21&1)+28|0}f[g+28>>2]=b;h=(b<<2)+31412|0;m:{n:{e=f[7778];c=1<<b;o:{if(!(e&c)){f[7778]=c|e;f[h>>2]=d;f[d+24>>2]=h;break o}e=a<<((b|0)==31?0:25-(b>>>1|0)|0);c=f[h>>2];while(1){b=c;if((f[c+4>>2]&-8)==(a|0)){break n}c=e>>>29|0;e=e<<1;h=b+(c&4)|0;c=f[h+16>>2];if(c){continue}break}f[h+16>>2]=d;f[d+24>>2]=b}f[d+12>>2]=d;f[d+8>>2]=d;break m}a=f[b+8>>2];f[a+12>>2]=d;f[b+8>>2]=d;f[d+24>>2]=0;f[d+12>>2]=b;f[d+8>>2]=a}a=f[7785]+ -1|0;f[7785]=a;if(a){break a}d=31564;while(1){a=f[d>>2];d=a+8|0;if(a){continue}break}f[7785]=-1}}function ha(a,b,c){var e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0;h=A-96|0;A=h;h=Q(h,0,96);d[h+32|0]=1;d[h|0]=1;e=(c&128)>>>7|0;i=0-e|0;J=i^c-e;K=b<<3;L=h- -64|0;M=h+32|0;j=f[h+32>>2];k=f[h>>2];b=0;while(1){c=l(b+K|0,96);b=b+1|0;g=(J^b)+ -1>>>31|0;e=0-g|0;g=g+ -1|0;m=f[c+1164>>2]&e|g&m;n=e&f[c+1160>>2]|g&n;p=e&f[c+1156>>2]|g&p;o=e&f[c+1152>>2]|g&o;q=e&f[c+1148>>2]|g&q;r=e&f[c+1144>>2]|g&r;s=e&f[c+1140>>2]|g&s;t=e&f[c+1136>>2]|g&t;u=e&f[c+1132>>2]|g&u;v=e&f[c+1128>>2]|g&v;w=e&f[c+1124>>2]|g&w;x=e&f[c+1120>>2]|g&x;y=e&f[c+1116>>2]|g&y;z=e&f[c+1112>>2]|g&z;B=e&f[c+1108>>2]|g&B;j=e&f[c+1104>>2]|g&j;C=e&f[c+1100>>2]|g&C;D=e&f[c+1096>>2]|g&D;E=e&f[c+1092>>2]|g&E;F=e&f[c+1088>>2]|g&F;G=e&f[c+1084>>2]|g&G;H=e&f[c+1080>>2]|g&H;I=e&f[c+1076>>2]|g&I;k=e&f[c+1072>>2]|g&k;if((b|0)!=8){continue}break}f[h+92>>2]=m;f[h+88>>2]=n;f[h+84>>2]=p;f[h+80>>2]=o;f[h+76>>2]=q;f[h+72>>2]=r;f[h+68>>2]=s;f[h+64>>2]=t;f[h+60>>2]=u;f[h+56>>2]=v;f[h+52>>2]=w;f[h+48>>2]=x;f[h+44>>2]=y;f[h+40>>2]=z;f[h+36>>2]=B;f[h+32>>2]=j;f[h+28>>2]=C;f[h+24>>2]=D;f[h+20>>2]=E;f[h+16>>2]=F;f[h+12>>2]=G;f[h+8>>2]=H;f[h+4>>2]=I;f[h>>2]=k;ga(a,h);ga(a+40|0,M);ga(a+80|0,L);c=f[a+40>>2];e=f[a>>2];b=(c^e)&i;f[a+40>>2]=b^c;f[a>>2]=b^e;c=f[a+44>>2];e=f[a+4>>2];b=(c^e)&i;f[a+4>>2]=b^e;f[a+44>>2]=b^c;c=f[a+48>>2];e=f[a+8>>2];b=(c^e)&i;f[a+8>>2]=b^e;f[a+48>>2]=b^c;c=f[a+52>>2];e=f[a+12>>2];b=(c^e)&i;f[a+12>>2]=b^e;f[a+52>>2]=b^c;c=f[a+56>>2];e=f[a+16>>2];b=(c^e)&i;f[a+16>>2]=b^e;f[a+56>>2]=b^c;c=f[a+60>>2];e=f[a+20>>2];b=(c^e)&i;f[a+60>>2]=b^c;f[a+20>>2]=b^e;c=a- -64|0;e=f[c>>2];g=f[a+24>>2];b=(e^g)&i;f[c>>2]=b^e;f[a+24>>2]=b^g;c=f[a+68>>2];e=f[a+28>>2];b=(c^e)&i;f[a+68>>2]=b^c;f[a+28>>2]=b^e;c=f[a+72>>2];e=f[a+32>>2];b=(c^e)&i;f[a+72>>2]=b^c;f[a+32>>2]=b^e;c=f[a+76>>2];e=f[a+36>>2];b=(c^e)&i;f[a+76>>2]=b^c;f[a+36>>2]=b^e;b=f[a+116>>2];c=f[a+112>>2];e=f[a+108>>2];g=f[a+104>>2];j=f[a+100>>2];k=f[a+96>>2];m=f[a+92>>2];n=f[a+88>>2];p=f[a+80>>2];q=134217690-p|0;o=f[a+84>>2];r=((q>>>26|0)-o|0)+67108862|0;f[a+84>>2]=(r&33554431^o)&i^o;o=((r>>>25|0)-n|0)+134217726|0;f[a+88>>2]=n^(n^o&67108863)&i;n=((o>>>26|0)-m|0)+67108862|0;f[a+92>>2]=m^(m^n&33554431)&i;m=((n>>>25|0)-k|0)+134217726|0;f[a+96>>2]=k^(k^m&67108863)&i;k=((m>>>26|0)-j|0)+67108862|0;f[a+100>>2]=j^(j^k&33554431)&i;j=((k>>>25|0)-g|0)+134217726|0;f[a+104>>2]=g^(g^j&67108863)&i;g=((j>>>26|0)-e|0)+67108862|0;f[a+108>>2]=e^(e^g&33554431)&i;e=((g>>>25|0)-c|0)+134217726|0;f[a+112>>2]=c^(c^e&67108863)&i;c=((e>>>26|0)-b|0)+67108862|0;f[a+116>>2]=b^(b^c&33554431)&i;f[a+80>>2]=p^(p^l(c>>>25|0,19)+(q&67108863))&i;A=h+96|0}function va(a,b,c){var d=0,e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,C=0,D=0,E=0,F=0,G=0;e=f[a+36>>2];i=f[a+32>>2];k=f[a+28>>2];j=f[a+24>>2];h=f[a+20>>2];if(c>>>0>=16){z=!g[a+76|0]<<24;n=f[a+4>>2];A=l(n,5);t=f[a+8>>2];y=l(t,5);w=f[a+12>>2];x=l(w,5);d=f[a+16>>2];u=l(d,5);C=d;o=f[a>>2];while(1){j=((g[b+3|0]|g[b+4|0]<<8|(g[b+5|0]<<16|g[b+6|0]<<24))>>>2&67108863)+j|0;d=kc(j,0,w);m=B;p=((g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24))&67108863)+h|0;v=kc(p,0,C);h=v+d|0;d=B+m|0;q=((g[b+6|0]|g[b+7|0]<<8|(g[b+8|0]<<16|g[b+9|0]<<24))>>>4&67108863)+k|0;m=kc(q,0,t);k=m+h|0;h=B+(h>>>0<v>>>0?d+1|0:d)|0;h=k>>>0<m>>>0?h+1|0:h;d=k;r=((g[b+9|0]|g[b+10|0]<<8|(g[b+11|0]<<16|g[b+12|0]<<24))>>>6|0)+i|0;k=kc(r,0,n);i=d+k|0;d=B+h|0;s=((g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24))>>>8|z)+e|0;h=kc(s,0,o);e=h+i|0;d=B+(i>>>0<k>>>0?d+1|0:d)|0;D=e;m=e>>>0<h>>>0?d+1|0:d;d=kc(j,0,t);e=B;h=kc(p,0,w);d=h+d|0;e=B+e|0;e=d>>>0<h>>>0?e+1|0:e;i=kc(q,0,n);h=i+d|0;d=B+e|0;d=h>>>0<i>>>0?d+1|0:d;i=kc(r,0,o);e=i+h|0;h=B+d|0;h=e>>>0<i>>>0?h+1|0:h;i=kc(s,0,u);e=i+e|0;d=B+h|0;E=e;v=e>>>0<i>>>0?d+1|0:d;d=kc(j,0,n);h=B;i=kc(p,0,t);e=i+d|0;d=B+h|0;d=e>>>0<i>>>0?d+1|0:d;i=kc(q,0,o);h=i+e|0;e=B+d|0;e=h>>>0<i>>>0?e+1|0:e;i=kc(r,0,u);h=i+h|0;d=B+e|0;d=h>>>0<i>>>0?d+1|0:d;i=kc(s,0,x);e=i+h|0;h=B+d|0;F=e;k=e>>>0<i>>>0?h+1|0:h;d=kc(j,0,o);h=B;i=kc(p,0,n);e=i+d|0;d=B+h|0;d=e>>>0<i>>>0?d+1|0:d;h=kc(q,0,u);e=h+e|0;d=B+d|0;d=e>>>0<h>>>0?d+1|0:d;i=kc(r,0,x);h=i+e|0;e=B+d|0;e=h>>>0<i>>>0?e+1|0:e;i=kc(s,0,y);h=i+h|0;d=B+e|0;G=h;i=h>>>0<i>>>0?d+1|0:d;d=kc(j,0,u);e=B;j=kc(p,0,o);d=j+d|0;h=B+e|0;h=d>>>0<j>>>0?h+1|0:h;j=kc(q,0,x);e=j+d|0;d=B+h|0;d=e>>>0<j>>>0?d+1|0:d;h=kc(r,0,y);e=h+e|0;d=B+d|0;d=e>>>0<h>>>0?d+1|0:d;j=kc(s,0,A);h=j+e|0;e=B+d|0;e=h>>>0<j>>>0?e+1|0:e;j=h;d=i;e=(e&67108863)<<6|h>>>26;h=e+G|0;if(h>>>0<e>>>0){d=d+1|0}i=h;h=k;d=(d&67108863)<<6|i>>>26;e=d+F|0;if(e>>>0<d>>>0){h=h+1|0}k=e;e=v;d=(h&67108863)<<6|k>>>26;h=d+E|0;if(h>>>0<d>>>0){e=e+1|0}d=m;e=(e&67108863)<<6|h>>>26;m=e+D|0;if(m>>>0<e>>>0){d=d+1|0}e=m;d=l((d&67108863)<<6|e>>>26,5)+(j&67108863)|0;j=(i&67108863)+(d>>>26|0)|0;k=k&67108863;i=h&67108863;e=e&67108863;h=d&67108863;b=b+16|0;c=c+ -16|0;if(c>>>0>15){continue}break}}f[a+36>>2]=e;f[a+32>>2]=i;f[a+28>>2]=k;f[a+24>>2]=j;f[a+20>>2]=h}function db(a,b,c,e,f,h){var i=0;i=A+ -64|0;A=i;ra(b,c,a,i);ka(e,f,i,h,64);oa(i);b=g[a+92|0]|g[a+93|0]<<8|(g[a+94|0]<<16|g[a+95|0]<<24);c=g[a+88|0]|g[a+89|0]<<8|(g[a+90|0]<<16|g[a+91|0]<<24);d[h+88|0]=c;d[h+89|0]=c>>>8;d[h+90|0]=c>>>16;d[h+91|0]=c>>>24;d[h+92|0]=b;d[h+93|0]=b>>>8;d[h+94|0]=b>>>16;d[h+95|0]=b>>>24;b=g[a+84|0]|g[a+85|0]<<8|(g[a+86|0]<<16|g[a+87|0]<<24);c=g[a+80|0]|g[a+81|0]<<8|(g[a+82|0]<<16|g[a+83|0]<<24);d[h+80|0]=c;d[h+81|0]=c>>>8;d[h+82|0]=c>>>16;d[h+83|0]=c>>>24;d[h+84|0]=b;d[h+85|0]=b>>>8;d[h+86|0]=b>>>16;d[h+87|0]=b>>>24;b=g[a+76|0]|g[a+77|0]<<8|(g[a+78|0]<<16|g[a+79|0]<<24);c=g[a+72|0]|g[a+73|0]<<8|(g[a+74|0]<<16|g[a+75|0]<<24);d[h+72|0]=c;d[h+73|0]=c>>>8;d[h+74|0]=c>>>16;d[h+75|0]=c>>>24;d[h+76|0]=b;d[h+77|0]=b>>>8;d[h+78|0]=b>>>16;d[h+79|0]=b>>>24;b=g[a+68|0]|g[a+69|0]<<8|(g[a+70|0]<<16|g[a+71|0]<<24);c=g[a+64|0]|g[a+65|0]<<8|(g[a+66|0]<<16|g[a+67|0]<<24);d[h+64|0]=c;d[h+65|0]=c>>>8;d[h+66|0]=c>>>16;d[h+67|0]=c>>>24;d[h+68|0]=b;d[h+69|0]=b>>>8;d[h+70|0]=b>>>16;d[h+71|0]=b>>>24;b=g[a+100|0]|g[a+101|0]<<8|(g[a+102|0]<<16|g[a+103|0]<<24);c=g[a+96|0]|g[a+97|0]<<8|(g[a+98|0]<<16|g[a+99|0]<<24);d[h+96|0]=c;d[h+97|0]=c>>>8;d[h+98|0]=c>>>16;d[h+99|0]=c>>>24;d[h+100|0]=b;d[h+101|0]=b>>>8;d[h+102|0]=b>>>16;d[h+103|0]=b>>>24;b=g[a+108|0]|g[a+109|0]<<8|(g[a+110|0]<<16|g[a+111|0]<<24);c=g[a+104|0]|g[a+105|0]<<8|(g[a+106|0]<<16|g[a+107|0]<<24);d[h+104|0]=c;d[h+105|0]=c>>>8;d[h+106|0]=c>>>16;d[h+107|0]=c>>>24;d[h+108|0]=b;d[h+109|0]=b>>>8;d[h+110|0]=b>>>16;d[h+111|0]=b>>>24;b=g[a+116|0]|g[a+117|0]<<8|(g[a+118|0]<<16|g[a+119|0]<<24);c=g[a+112|0]|g[a+113|0]<<8|(g[a+114|0]<<16|g[a+115|0]<<24);d[h+112|0]=c;d[h+113|0]=c>>>8;d[h+114|0]=c>>>16;d[h+115|0]=c>>>24;d[h+116|0]=b;d[h+117|0]=b>>>8;d[h+118|0]=b>>>16;d[h+119|0]=b>>>24;b=g[a+124|0]|g[a+125|0]<<8|(g[a+126|0]<<16|g[a+127|0]<<24);a=g[a+120|0]|g[a+121|0]<<8|(g[a+122|0]<<16|g[a+123|0]<<24);d[h+120|0]=a;d[h+121|0]=a>>>8;d[h+122|0]=a>>>16;d[h+123|0]=a>>>24;d[h+124|0]=b;d[h+125|0]=b>>>8;d[h+126|0]=b>>>16;d[h+127|0]=b>>>24;A=i- -64|0}function Na(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,B=0;c=A-192|0;A=c;vb(c+128|0,b);ha(c+8|0,0,d[c+129|0]);g=f[c+48>>2];e=f[c+8>>2];b=(g-e|0)+268435380|0;q=b&67108863;f[a>>2]=q;i=f[c+52>>2];j=f[c+12>>2];b=((i+(b>>>26|0)|0)-j|0)+134217724|0;f[a+4>>2]=b&33554431;k=f[c+56>>2];m=f[c+16>>2];b=(k-m+(b>>>25)|0)+268435452|0;f[a+8>>2]=b&67108863;n=f[c+60>>2];o=f[c+20>>2];b=(n-o+(b>>>26)|0)+134217724|0;f[a+12>>2]=b&33554431;p=f[c- -64>>2];r=f[c+24>>2];b=(p-r+(b>>>25)|0)+268435452|0;f[a+16>>2]=b&67108863;s=f[c+68>>2];t=f[c+28>>2];b=(s-t+(b>>>26)|0)+134217724|0;f[a+20>>2]=b&33554431;u=f[c+72>>2];v=f[c+32>>2];b=(u-v+(b>>>25)|0)+268435452|0;f[a+24>>2]=b&67108863;w=f[c+76>>2];x=f[c+36>>2];b=(w-x+(b>>>26)|0)+134217724|0;f[a+28>>2]=b&33554431;y=f[c+80>>2];z=f[c+40>>2];B=(y-z+(b>>>25)|0)+268435452|0;f[a+32>>2]=B&67108863;b=f[c+84>>2];h=f[c+44>>2];g=g+e|0;e=j+(i+(g>>>26|0)|0)|0;i=m+(k+(e>>>25|0)|0)|0;j=n+o+(i>>>26)|0;k=p+r+(j>>>25)|0;m=s+t+(k>>>26)|0;n=u+v+(m>>>25)|0;o=w+x+(n>>>26)|0;p=y+z+(o>>>25)|0;f[a+72>>2]=p&67108863;f[a+68>>2]=o&33554431;f[a- -64>>2]=n&67108863;f[a+60>>2]=m&33554431;f[a+56>>2]=k&67108863;f[a+52>>2]=j&33554431;f[a+48>>2]=i&67108863;f[a+44>>2]=e&33554431;e=b+h+(p>>>26)|0;f[a+76>>2]=e&33554431;b=(b-h+(B>>>26)|0)+134217724|0;f[a+36>>2]=b&33554431;f[a+40>>2]=l(e>>>25|0,19)+(g&67108863);f[a>>2]=l(b>>>25|0,19)+q;Q(a+84|0,0,36);f[a+120>>2]=f[c+88>>2];f[a+124>>2]=f[c+92>>2];f[a+128>>2]=f[c+96>>2];f[a+132>>2]=f[c+100>>2];f[a+136>>2]=f[c+104>>2];f[a+140>>2]=f[c+108>>2];f[a+144>>2]=f[c+112>>2];f[a+148>>2]=f[c+116>>2];f[a+152>>2]=f[c+120>>2];b=f[c+124>>2];f[a+80>>2]=2;f[a+156>>2]=b;h=c+88|0;b=3;while(1){ha(c+8|0,b>>>1|0,d[(c+128|0)+b|0]);ua(a,c+8|0);g=b>>>0<62;b=b+2|0;if(g){continue}break}ta(a,a);ta(a,a);ta(a,a);La(a,a);ha(c+8|0,0,d[c+128|0]);N(h,h,25648);ua(a,c+8|0);b=2;while(1){ha(c+8|0,b>>>1|0,d[(c+128|0)+b|0]);ua(a,c+8|0);h=b>>>0<62;b=b+2|0;if(h){continue}break}A=c+192|0}function ca(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0;n=f[b+36>>2];e=f[b+32>>2];g=f[b+28>>2];h=f[b+24>>2];i=f[b+20>>2];j=f[b+16>>2];k=f[b+12>>2];m=f[b+8>>2];c=f[b+4>>2];b=f[b>>2];c=c+(b>>>26|0)|0;m=m+(c>>>25|0)|0;k=k+(m>>>26|0)|0;j=j+(k>>>25|0)|0;i=i+(j>>>26|0)|0;h=h+(i>>>25|0)|0;g=g+(h>>>26|0)|0;e=e+(g>>>25|0)|0;n=n+(e>>>26|0)|0;b=l(n>>>25|0,19)+(b&67108863)|0;o=b&67108863;b=(c&33554431)+(b>>>26|0)|0;c=(m&67108863)+(b>>>25|0)|0;m=(k&33554431)+(c>>>26|0)|0;k=(j&67108863)+(m>>>25|0)|0;j=(i&33554431)+(k>>>26|0)|0;i=(h&67108863)+(j>>>25|0)|0;h=(g&33554431)+(i>>>26|0)|0;g=(e&67108863)+(h>>>25|0)|0;e=(n&33554431)+(g>>>26|0)|0;n=(o+l(e>>>25|0,19)|0)+19|0;o=n&67108863;p=e&33554431;e=g&67108863;g=h&33554431;h=i&67108863;i=j&33554431;j=k&67108863;k=m&33554431;m=c&67108863;c=(b&33554431)+(n>>>26|0)|0;m=m+(c>>>25|0)|0;k=k+(m>>>26|0)|0;j=j+(k>>>25|0)|0;i=i+(j>>>26|0)|0;h=h+(i>>>25|0)|0;g=g+(h>>>26|0)|0;e=e+(g>>>25|0)|0;n=p+(e>>>26|0)|0;b=(o+l(n>>>25|0,19)|0)+67108845|0;d[a|0]=b;d[a+2|0]=b>>>16;d[a+1|0]=b>>>8;c=((c&33554431)+(b>>>26|0)|0)+33554431|0;d[a+5|0]=c>>>14;d[a+4|0]=c>>>6;d[a+3|0]=b>>>24&3|c<<2;b=((m&67108863)+(c>>>25|0)|0)+67108863|0;d[a+8|0]=b>>>13;d[a+7|0]=b>>>5;d[a+6|0]=c>>>22&7|b<<3;c=((k&33554431)+(b>>>26|0)|0)+33554431|0;d[a+11|0]=c>>>11;d[a+10|0]=c>>>3;d[a+9|0]=b>>>21&31|c<<5;b=((j&67108863)+(c>>>25|0)|0)+67108863|0;d[a+15|0]=b>>>18;d[a+14|0]=b>>>10;d[a+13|0]=b>>>2;d[a+12|0]=c>>>19&63|b<<6;b=((i&33554431)+(b>>>26|0)|0)+33554431|0;d[a+16|0]=b;d[a+18|0]=b>>>16;d[a+17|0]=b>>>8;c=((h&67108863)+(b>>>25|0)|0)+67108863|0;d[a+21|0]=c>>>15;d[a+20|0]=c>>>7;d[a+19|0]=b>>>24&1|c<<1;b=((g&33554431)+(c>>>26|0)|0)+33554431|0;d[a+24|0]=b>>>13;d[a+23|0]=b>>>5;d[a+22|0]=c>>>23&7|b<<3;c=((e&67108863)+(b>>>25|0)|0)+67108863|0;d[a+27|0]=c>>>12;d[a+26|0]=c>>>4;d[a+25|0]=b>>>21&15|c<<4;b=(n+(c>>>26|0)|0)+33554431|0;d[a+30|0]=b>>>10;d[a+29|0]=b>>>2;d[a+31|0]=b>>>18&127;d[a+28|0]=c>>>20&63|b<<6}function Z(a,b,c){var d=0,e=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0;d=A-112|0;A=d;Q((d+48|0)+c|0,0,c>>>0>63?0:64-c|0);P(d+48|0,b,c);c=g[d+48|0]|g[d+49|0]<<8|(g[d+50|0]<<16|g[d+51|0]<<24);b=d+48|4;i=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);b=d+48|8;j=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);b=d+48|12;k=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);b=d- -64|0;b=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);l=g[d+68|0]|g[d+69|0]<<8|(g[d+70|0]<<16|g[d+71|0]<<24);m=g[d+72|0]|g[d+73|0]<<8|(g[d+74|0]<<16|g[d+75|0]<<24);n=g[d+84|0]|g[d+85|0]<<8|(g[d+86|0]<<16|g[d+87|0]<<24);o=g[d+88|0]|g[d+89|0]<<8|(g[d+90|0]<<16|g[d+91|0]<<24);p=g[d+92|0]|g[d+93|0]<<8|(g[d+94|0]<<16|g[d+95|0]<<24);q=g[d+96|0]|g[d+97|0]<<8|(g[d+98|0]<<16|g[d+99|0]<<24);r=g[d+100|0]|g[d+101|0]<<8|(g[d+102|0]<<16|g[d+103|0]<<24);s=g[d+104|0]|g[d+105|0]<<8|(g[d+106|0]<<16|g[d+107|0]<<24);t=g[d+108|0]|g[d+109|0]<<8|(g[d+110|0]<<16|g[d+111|0]<<24);e=g[d+80|0]|g[d+81|0]<<8|(g[d+82|0]<<16|g[d+83|0]<<24);h=g[d+76|0]|g[d+77|0]<<8|(g[d+78|0]<<16|g[d+79|0]<<24);f[a+32>>2]=e<<16&16711680|h>>>16;f[a+28>>2]=h<<14&1073725440|m>>>18;f[a+24>>2]=m<<12&1073737728|l>>>20;f[a+20>>2]=l<<10&1073740800|b>>>22;f[a+16>>2]=b<<8&1073741568|k>>>24;f[a+12>>2]=k<<6&1073741760|j>>>26;f[a+8>>2]=j<<4&1073741808|i>>>28;f[a+4>>2]=i<<2&1073741820|c>>>30;f[a>>2]=c&1073741823;f[d+32>>2]=t>>>8;f[d+20>>2]=r<<18&1073479680|q>>>14;f[d+16>>2]=q<<16&1073676288|p>>>16;f[d+12>>2]=p<<14&1073725440|o>>>18;f[d+8>>2]=o<<12&1073737728|n>>>20;f[d+4>>2]=n<<10&1073740800|e>>>22;f[d>>2]=e<<8&1073741568|h>>>24;f[d+24>>2]=s<<20&1072693248|r>>>12;f[d+28>>2]=t<<22&1069547520|s>>>10;Ma(a,d,a);A=d+112|0}function Oa(a,b,c,e,h){var i=0;i=A-32|0;A=i;ja(c,i);ka(a,b,c,h,64);b=f[i+28>>2];a=f[i+24>>2];d[h+88|0]=a;d[h+89|0]=a>>>8;d[h+90|0]=a>>>16;d[h+91|0]=a>>>24;d[h+92|0]=b;d[h+93|0]=b>>>8;d[h+94|0]=b>>>16;d[h+95|0]=b>>>24;b=f[i+20>>2];a=f[i+16>>2];d[h+80|0]=a;d[h+81|0]=a>>>8;d[h+82|0]=a>>>16;d[h+83|0]=a>>>24;d[h+84|0]=b;d[h+85|0]=b>>>8;d[h+86|0]=b>>>16;d[h+87|0]=b>>>24;b=f[i+12>>2];a=f[i+8>>2];d[h+72|0]=a;d[h+73|0]=a>>>8;d[h+74|0]=a>>>16;d[h+75|0]=a>>>24;d[h+76|0]=b;d[h+77|0]=b>>>8;d[h+78|0]=b>>>16;d[h+79|0]=b>>>24;b=f[i+4>>2];a=f[i>>2];d[h+64|0]=a;d[h+65|0]=a>>>8;d[h+66|0]=a>>>16;d[h+67|0]=a>>>24;d[h+68|0]=b;d[h+69|0]=b>>>8;d[h+70|0]=b>>>16;d[h+71|0]=b>>>24;b=g[e+4|0]|g[e+5|0]<<8|(g[e+6|0]<<16|g[e+7|0]<<24);a=g[e|0]|g[e+1|0]<<8|(g[e+2|0]<<16|g[e+3|0]<<24);d[h+96|0]=a;d[h+97|0]=a>>>8;d[h+98|0]=a>>>16;d[h+99|0]=a>>>24;d[h+100|0]=b;d[h+101|0]=b>>>8;d[h+102|0]=b>>>16;d[h+103|0]=b>>>24;b=g[e+12|0]|g[e+13|0]<<8|(g[e+14|0]<<16|g[e+15|0]<<24);a=g[e+8|0]|g[e+9|0]<<8|(g[e+10|0]<<16|g[e+11|0]<<24);d[h+104|0]=a;d[h+105|0]=a>>>8;d[h+106|0]=a>>>16;d[h+107|0]=a>>>24;d[h+108|0]=b;d[h+109|0]=b>>>8;d[h+110|0]=b>>>16;d[h+111|0]=b>>>24;b=g[e+20|0]|g[e+21|0]<<8|(g[e+22|0]<<16|g[e+23|0]<<24);a=g[e+16|0]|g[e+17|0]<<8|(g[e+18|0]<<16|g[e+19|0]<<24);d[h+112|0]=a;d[h+113|0]=a>>>8;d[h+114|0]=a>>>16;d[h+115|0]=a>>>24;d[h+116|0]=b;d[h+117|0]=b>>>8;d[h+118|0]=b>>>16;d[h+119|0]=b>>>24;b=g[e+28|0]|g[e+29|0]<<8|(g[e+30|0]<<16|g[e+31|0]<<24);a=g[e+24|0]|g[e+25|0]<<8|(g[e+26|0]<<16|g[e+27|0]<<24);d[h+120|0]=a;d[h+121|0]=a>>>8;d[h+122|0]=a>>>16;d[h+123|0]=a>>>24;d[h+124|0]=b;d[h+125|0]=b>>>8;d[h+126|0]=b>>>16;d[h+127|0]=b>>>24;A=i+32|0}function X(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0;j=A-48|0;A=j;if((c|0)>0){while(1){h=e<<3;d=h+a|0;g=f[d>>2];h=b+h|0;i=f[h>>2];h=f[d+4>>2]^f[h+4>>2];f[d>>2]=g^i;f[d+4>>2]=h;e=e+1|0;if((e|0)!=(c|0)){continue}break}}m=0;while(1){e=0;while(1){d=e<<3;b=d+a|0;h=f[b+40>>2];g=f[b+164>>2]^(f[b+124>>2]^(f[b+84>>2]^(f[b+44>>2]^f[b+4>>2])));d=d+j|0;f[d>>2]=f[b+160>>2]^(f[b+120>>2]^(f[b+80>>2]^(f[b>>2]^h)));f[d+4>>2]=g;c=0;e=e+1|0;if((e|0)!=5){continue}break}while(1){b=c;c=b+1|0;d=(((c&255)>>>0)%5<<3)+j|0;i=Wa(f[d>>2],f[d+4>>2],1);d=(((b+4&255)>>>0)%5<<3)+j|0;h=i^f[d>>2];g=B^f[d+4>>2];e=0;while(1){d=(b+e<<3)+a|0;n=f[d>>2];i=f[d+4>>2]^g;f[d>>2]=h^n;f[d+4>>2]=i;d=e>>>0<20;e=e+5|0;if(d){continue}break}if((c|0)!=5){continue}break}b=f[a+8>>2];c=f[a+12>>2];e=0;while(1){i=e<<2;g=(f[i+30432>>2]<<3)+a|0;d=f[g>>2];h=f[g+4>>2];f[g>>2]=Wa(b,c,f[i+30528>>2]);f[g+4>>2]=B;b=d;c=h;e=e+1|0;if((e|0)!=24){continue}break}f[j>>2]=d;f[j+4>>2]=h;h=0;c=0;while(1){d=P(j,l(h,40)+a|0,40);b=(c<<3)+a|0;g=f[b>>2];i=f[d+12>>2];n=i;e=f[d+20>>2];i=f[b+4>>2]^e&(i^-1);k=g;g=f[d+16>>2];o=f[d+8>>2];f[b>>2]=k^g&(o^-1);f[b+4>>2]=i;i=f[d+28>>2];e=f[b+12>>2]^i&(e^-1);k=f[d+24>>2];f[b+8>>2]=f[b+8>>2]^k&(g^-1);f[b+12>>2]=e;e=f[d+36>>2];g=f[b+20>>2]^e&(i^-1);i=f[d+32>>2];f[b+16>>2]=f[b+16>>2]^i&(k^-1);f[b+20>>2]=g;g=f[d+4>>2];e=f[b+28>>2]^g&(e^-1);k=f[d>>2];f[b+24>>2]=f[b+24>>2]^k&(i^-1);f[b+28>>2]=e;e=f[b+36>>2]^(g^-1)&n;f[b+32>>2]=f[b+32>>2]^(k^-1)&o;f[b+36>>2]=e;c=c+5|0;h=h+1|0;if((h|0)!=5){continue}break}b=(m<<3)+30624|0;c=f[b>>2];b=f[a+4>>2]^f[b+4>>2];f[a>>2]=c^f[a>>2];f[a+4>>2]=b;m=m+1|0;if((m|0)!=24){continue}break}A=d+48|0}function Ea(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0,A=0,B=0,C=0,D=0,E=0,F=0,G=0,H=0,I=0,J=0,K=0;v=f[c>>2];q=v;w=f[c+4>>2];i=w;x=f[c+8>>2];j=x;y=f[c+12>>2];k=y;z=f[c+16>>2];l=z;A=f[c+20>>2];m=A;B=f[c+24>>2];n=B;C=f[c+28>>2];o=C;D=f[c+32>>2];e=D;E=f[c+36>>2];g=E;F=f[c+40>>2];h=F;G=f[c+44>>2];p=G;H=f[c+48>>2];d=H;I=f[c+52>>2];s=I;J=f[c+56>>2];r=J;K=f[c+60>>2];c=K;if((a|0)>=1){c=K;r=J;s=I;d=H;p=G;h=F;g=E;e=D;o=C;n=B;m=A;l=z;k=y;j=x;i=w;q=v;while(1){t=l;l=l+q|0;q=O(l^d,16);e=q+e|0;d=O(t^e,12);t=e;e=q;q=l+d|0;e=O(e^q,8);t=t+e|0;l=O(d^t,7);d=g;i=m+i|0;g=O(i^s,16);d=d+g|0;m=O(d^m,12);i=m+i|0;g=O(i^g,8);u=g+d|0;m=O(m^u,7);d=n;n=n+j|0;j=O(n^r,16);h=j+h|0;d=O(d^h,12);r=j;j=n+d|0;r=O(r^j,8);h=r+h|0;n=O(d^h,7);d=o;s=c;c=o+k|0;o=O(s^c,16);k=o+p|0;p=O(d^k,12);d=p;s=k;k=c+p|0;c=O(o^k,8);p=s+c|0;o=O(d^p,7);d=h;h=c;c=m+q|0;h=O(h^c,16);d=d+h|0;m=O(d^m,12);q=c+m|0;c=O(h^q,8);h=d+c|0;m=O(m^h,7);i=n+i|0;e=O(i^e,16);p=e+p|0;n=O(p^n,12);i=n+i|0;d=O(e^i,8);p=p+d|0;n=O(n^p,7);j=o+j|0;e=O(j^g,16);g=e+t|0;o=O(g^o,12);j=o+j|0;s=O(e^j,8);e=g+s|0;o=O(o^e,7);k=l+k|0;g=O(k^r,16);t=g+u|0;l=O(t^l,12);u=(a|0)>2;k=l+k|0;r=O(g^k,8);g=t+r|0;l=O(l^g,7);a=a+ -2|0;if(u){continue}break}}f[b+60>>2]=c+K;f[b+56>>2]=r+J;f[b+52>>2]=s+I;f[b+48>>2]=d+H;f[b+44>>2]=p+G;f[b+40>>2]=h+F;f[b+36>>2]=g+E;f[b+32>>2]=e+D;f[b+28>>2]=o+C;f[b+24>>2]=n+B;f[b+20>>2]=m+A;f[b+16>>2]=l+z;f[b+12>>2]=k+y;f[b+8>>2]=j+x;f[b+4>>2]=i+w;f[b>>2]=q+v}function nb(a,b){var c=0;c=(f[b+40>>2]-f[b>>2]|0)+134217690|0;f[a>>2]=c&67108863;c=((f[b+44>>2]+(c>>>26|0)|0)-f[b+4>>2]|0)+67108862|0;f[a+4>>2]=c&33554431;c=(f[b+48>>2]-f[b+8>>2]+(c>>>25)|0)+134217726|0;f[a+8>>2]=c&67108863;c=(f[b+52>>2]-f[b+12>>2]+(c>>>26)|0)+67108862|0;f[a+12>>2]=c&33554431;f[a+16>>2]=(f[b+56>>2]-f[b+16>>2]+(c>>>25)|0)+134217726;f[a+20>>2]=(f[b+60>>2]-f[b+20>>2]|0)+67108862;c=b- -64|0;f[a+24>>2]=(f[c>>2]-f[b+24>>2]|0)+134217726;f[a+28>>2]=(f[b+68>>2]-f[b+28>>2]|0)+67108862;f[a+32>>2]=(f[b+72>>2]-f[b+32>>2]|0)+134217726;f[a+36>>2]=(f[b+76>>2]-f[b+36>>2]|0)+67108862;f[a+40>>2]=f[b>>2]+f[b+40>>2];f[a+44>>2]=f[b+4>>2]+f[b+44>>2];f[a+48>>2]=f[b+8>>2]+f[b+48>>2];f[a+52>>2]=f[b+12>>2]+f[b+52>>2];f[a+56>>2]=f[b+16>>2]+f[b+56>>2];f[a+60>>2]=f[b+20>>2]+f[b+60>>2];f[a- -64>>2]=f[b+24>>2]+f[c>>2];f[a+68>>2]=f[b+28>>2]+f[b+68>>2];f[a+72>>2]=f[b+32>>2]+f[b+72>>2];f[a+76>>2]=f[b+36>>2]+f[b+76>>2];f[a+80>>2]=f[b+80>>2];f[a+84>>2]=f[b+84>>2];f[a+88>>2]=f[b+88>>2];f[a+92>>2]=f[b+92>>2];f[a+96>>2]=f[b+96>>2];f[a+100>>2]=f[b+100>>2];f[a+104>>2]=f[b+104>>2];f[a+108>>2]=f[b+108>>2];f[a+112>>2]=f[b+112>>2];f[a+116>>2]=f[b+116>>2];N(a+120|0,b+120|0,29616)}function P(a,b,c){var e=0,h=0,i=0;if(c>>>0>=512){y(a|0,b|0,c|0)|0;return a}h=a+c|0;a:{if(!((a^b)&3)){b:{if((c|0)<1){c=a;break b}if(!(a&3)){c=a;break b}c=a;while(1){d[c|0]=g[b|0];b=b+1|0;c=c+1|0;if(c>>>0>=h>>>0){break b}if(c&3){continue}break}}e=h&-4;c:{if(e>>>0<64){break c}i=e+ -64|0;if(c>>>0>i>>>0){break c}while(1){f[c>>2]=f[b>>2];f[c+4>>2]=f[b+4>>2];f[c+8>>2]=f[b+8>>2];f[c+12>>2]=f[b+12>>2];f[c+16>>2]=f[b+16>>2];f[c+20>>2]=f[b+20>>2];f[c+24>>2]=f[b+24>>2];f[c+28>>2]=f[b+28>>2];f[c+32>>2]=f[b+32>>2];f[c+36>>2]=f[b+36>>2];f[c+40>>2]=f[b+40>>2];f[c+44>>2]=f[b+44>>2];f[c+48>>2]=f[b+48>>2];f[c+52>>2]=f[b+52>>2];f[c+56>>2]=f[b+56>>2];f[c+60>>2]=f[b+60>>2];b=b- -64|0;c=c- -64|0;if(c>>>0<=i>>>0){continue}break}}if(c>>>0>=e>>>0){break a}while(1){f[c>>2]=f[b>>2];b=b+4|0;c=c+4|0;if(c>>>0<e>>>0){continue}break}break a}if(h>>>0<4){c=a;break a}e=h+ -4|0;if(e>>>0<a>>>0){c=a;break a}c=a;while(1){d[c|0]=g[b|0];d[c+1|0]=g[b+1|0];d[c+2|0]=g[b+2|0];d[c+3|0]=g[b+3|0];b=b+4|0;c=c+4|0;if(c>>>0<=e>>>0){continue}break}}if(c>>>0<h>>>0){while(1){d[c|0]=g[b|0];b=b+1|0;c=c+1|0;if((h|0)!=(c|0)){continue}break}}return a}function Vb(a,b){var c=0,e=0,g=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0;h=f[a+56>>2];if(h){d[(a+h|0)+60|0]=1;c=h+1|0;if(c>>>0<=15){Q((a+c|0)+60|0,0,15-h|0)}d[a+76|0]=1;va(a,a+60|0,16)}v=f[a+52>>2];w=f[a+48>>2];x=f[a+44>>2];k=f[a+24>>2];i=f[a+28>>2]+(k>>>26|0)|0;e=f[a+32>>2]+(i>>>26|0)|0;o=f[a+36>>2]+(e>>>26|0)|0;c=f[a+20>>2]+l(o>>>26|0,5)|0;g=c&67108863;h=g+5|0;r=e&67108863;m=i&67108863;e=(k&67108863)+(c>>>26|0)|0;c=e+(h>>>26|0)|0;k=m+(c>>>26|0)|0;s=r+(k>>>26|0)|0;p=(o|-67108864)+(s>>>26|0)|0;n=p>>31;t=(p>>>31|0)+ -1|0;i=t&67108863;u=n&e|i&c;j=b;c=0;g=g&n|h&i|u<<26;e=g+f[a+40>>2]|0;if(e>>>0<g>>>0){c=1}U(j,e);q=b+4|0;h=0;m=n&m|i&k;g=m<<20|u>>>6;e=g+x|0;if(e>>>0<g>>>0){h=1}j=e;e=c;g=j+e|0;c=h;c=g>>>0<e>>>0?c+1|0:c;U(q,g);q=b+8|0;h=0;i=n&r|i&s;g=i<<14|m>>>12;e=g+w|0;if(e>>>0<g>>>0){h=1}j=e;e=c;g=j+e|0;c=h;c=g>>>0<e>>>0?c+1|0:c;U(q,g);j=b+12|0;h=0;e=(p&t|n&o)<<8|i>>>18;b=e+v|0;if(b>>>0<e>>>0){h=1}c=b+c|0;U(j,c);Q(a,0,56)}function bb(a,b,c,e){var h=0,i=0,j=0,k=0;j=A+ -64|0;A=j;a:{if(!e){break a}h=g[b+129|0];if(h){i=h>>>0<e>>>0?h:e;k=(i|0)>1?i:1;h=0;while(1){d[a+h|0]=g[((g[b+128|0]+h|0)+b|0)- -64|0]^g[c+h|0];h=h+1|0;if((k|0)!=(h|0)){continue}break}Q((g[b+128|0]+b|0)- -64|0,0,i);d[b+129|0]=g[b+129|0]-i;d[b+128|0]=i+g[b+128|0];e=e-i|0;if(!e){break a}c=c+i|0;a=a+i|0}if(e>>>0>=64){while(1){Ea(g[b+130|0],j,b);h=f[b+48>>2];i=h+1|0;f[b+48>>2]=i;if(i>>>0<h>>>0){f[b+52>>2]=f[b+52>>2]+1}h=0;while(1){d[a+h|0]=g[h+j|0]^g[c+h|0];h=h+1|0;if((h|0)!=64){continue}break}a=a- -64|0;c=c- -64|0;e=e+ -64|0;if(e>>>0>63){continue}break}if(!e){break a}}Ea(g[b+130|0],j,b);h=f[b+48>>2];i=h+1|0;f[b+48>>2]=i;if(i>>>0<h>>>0){f[b+52>>2]=f[b+52>>2]+1}i=e>>>0>1?e:1;h=0;while(1){d[a+h|0]=g[h+j|0]^g[c+h|0];h=h+1|0;if((i|0)!=(h|0)){continue}break}d[b+128|0]=i;d[b+129|0]=64-e;if(i>>>0>63){break a}P((b+i|0)- -64|0,i+j|0,64-i|0)}A=j- -64|0}function Mb(a,b,c,e,g,h){var i=0,j=0;i=A-560|0;A=i;j=1;if(!Ra(c)){Ua(i+140|0,c,h);ba(i+144|0,b,32);R(i+144|0,1026,1);R(i+144|0,a,32);R(i+144|0,i+140|0,4);aa(i+144|0,i- -64|0);Lb(e,i- -64|0,a,h);ba(i+144|0,b,32);R(i+144|0,1030,1);R(i+144|0,a,32);R(i+144|0,i+140|0,4);aa(i+144|0,i);a=f[i+60>>2];b=f[i+56>>2];d[g+24|0]=b;d[g+25|0]=b>>>8;d[g+26|0]=b>>>16;d[g+27|0]=b>>>24;d[g+28|0]=a;d[g+29|0]=a>>>8;d[g+30|0]=a>>>16;d[g+31|0]=a>>>24;a=f[i+52>>2];b=f[i+48>>2];d[g+16|0]=b;d[g+17|0]=b>>>8;d[g+18|0]=b>>>16;d[g+19|0]=b>>>24;d[g+20|0]=a;d[g+21|0]=a>>>8;d[g+22|0]=a>>>16;d[g+23|0]=a>>>24;a=f[i+44>>2];b=f[i+40>>2];d[g+8|0]=b;d[g+9|0]=b>>>8;d[g+10|0]=b>>>16;d[g+11|0]=b>>>24;d[g+12|0]=a;d[g+13|0]=a>>>8;d[g+14|0]=a>>>16;d[g+15|0]=a>>>24;a=f[i+36>>2];b=f[i+32>>2];d[g|0]=b;d[g+1|0]=b>>>8;d[g+2|0]=b>>>16;d[g+3|0]=b>>>24;d[g+4|0]=a;d[g+5|0]=a>>>8;d[g+6|0]=a>>>16;d[g+7|0]=a>>>24;j=0}A=i+560|0;return j}function pb(a,b,c,d){var e=0,h=0,i=0,j=0,k=0,m=0,n=0,o=0;e=A-2112|0;A=e;Ga(e+1856|0,c,5);Ga(e+1600|0,d,7);La(e+160|0,b);nb(e+320|0,b);b=0;while(1){c=(e+320|0)+l(b,160)|0;mb(c+160|0,e+160|0,c);b=b+1|0;if((b|0)!=7){continue}break}c=Q(a,0,160);f[c+80>>2]=1;f[c+40>>2]=1;m=c+80|0;n=c+40|0;a=255;a:{while(1){b=a;if(!(g[b+(e+1600|0)|0]|g[(e+1856|0)+b|0])){a=b+ -1|0;if(b){continue}break a}break}if((b|0)<0){break a}o=c+120|0;a=e+80|0;i=e+40|0;d=e+120|0;while(1){na(e,c);h=g[(e+1856|0)+b|0];if(h){N(c,e,d);N(n,i,a);N(m,a,d);N(o,e,i);j=h<<24>>24;k=j>>31;lb(e,c,(e+320|0)+l((k^k+j)>>>1|0,160)|0,h>>>7|0)}h=g[(e+1600|0)+b|0];if(h){N(c,e,d);N(n,i,a);N(m,a,d);N(o,e,i);j=h<<24>>24;k=j>>31;kb(e,c,l((k^k+j)>>>1|0,120)+25776|0,h>>>7|0)}N(c,e,d);N(n,i,a);N(m,a,d);h=(b|0)>0;b=b+ -1|0;if(h){continue}break}}A=e+2112|0}function Ga(a,b,c){var e=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0;j=-1<<c+ -1;c=a;while(1){h=f[(i<<2)+b>>2];e=0;while(1){d[c|0]=h&1;h=h>>>1|0;c=c+1|0;e=e+1|0;if((e|0)!=30){continue}break}i=i+1|0;if((i|0)!=8){continue}break}o=j^-1;h=f[b+32>>2];b=0;e=0;while(1){d[c|0]=h&1;h=h>>>1|0;c=c+1|0;e=e+1|0;if((e|0)!=16){continue}break}while(1){a:{i=a+b|0;e=g[i|0];if(!e|b>>>0>254){break a}p=256-b|0;h=1;while(1){c=b+h|0;k=c+a|0;l=d[k|0];m=l<<h;e=e<<24>>24;n=m+e|0;b:{if((n|0)<=(o|0)){d[i|0]=n;d[k|0]=0;break b}e=e-m|0;if((e|0)>(j|0)){d[i|0]=e;if(c>>>0>255){break b}while(1){e=a+c|0;if(!g[e|0]){d[e|0]=1;break b}d[e|0]=0;e=c>>>0<255;c=c+1|0;if(e){continue}break}break b}if(l){break a}}c=h>>>0>5;h=h+1|0;if(c|h>>>0>=p>>>0){break a}e=g[i|0];continue}}b=b+1|0;if((b|0)!=256){continue}break}}function Wb(a,b){var c=0;f[a>>2]=(g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24))&67108863;f[a+4>>2]=(g[b+3|0]|g[b+4|0]<<8|(g[b+5|0]<<16|g[b+6|0]<<24))>>>2&67108611;f[a+8>>2]=(g[b+6|0]|g[b+7|0]<<8|(g[b+8|0]<<16|g[b+9|0]<<24))>>>4&67092735;f[a+12>>2]=(g[b+9|0]|g[b+10|0]<<8|(g[b+11|0]<<16|g[b+12|0]<<24))>>>6&66076671;c=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[a+20>>2]=0;f[a+24>>2]=0;f[a+28>>2]=0;f[a+32>>2]=0;f[a+36>>2]=0;f[a+16>>2]=c>>>8&1048575;f[a+40>>2]=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);f[a+44>>2]=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[a+48>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);b=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);d[a+76|0]=0;f[a+56>>2]=0;f[a+52>>2]=b}function Q(a,b,c){a=a|0;b=b|0;c=c|0;var e=0,g=0,h=0,i=0;a:{if(!c){break a}e=a+c|0;d[e+ -1|0]=b;d[a|0]=b;if(c>>>0<3){break a}d[e+ -2|0]=b;d[a+1|0]=b;d[e+ -3|0]=b;d[a+2|0]=b;if(c>>>0<7){break a}d[e+ -4|0]=b;d[a+3|0]=b;if(c>>>0<9){break a}e=0-a&3;g=e+a|0;b=l(b&255,16843009);f[g>>2]=b;c=c-e&-4;e=c+g|0;f[e+ -4>>2]=b;if(c>>>0<9){break a}f[g+8>>2]=b;f[g+4>>2]=b;f[e+ -8>>2]=b;f[e+ -12>>2]=b;if(c>>>0<25){break a}f[g+24>>2]=b;f[g+20>>2]=b;f[g+16>>2]=b;f[g+12>>2]=b;f[e+ -16>>2]=b;f[e+ -20>>2]=b;f[e+ -24>>2]=b;f[e+ -28>>2]=b;i=g&4|24;c=c-i|0;if(c>>>0<32){break a}e=b;h=b;b=g+i|0;while(1){f[b+24>>2]=h;f[b+28>>2]=e;f[b+16>>2]=h;f[b+20>>2]=e;f[b+8>>2]=h;f[b+12>>2]=e;f[b>>2]=h;f[b+4>>2]=e;b=b+32|0;c=c+ -32|0;if(c>>>0>31){continue}break}}return a|0}function cb(a,b,c){f[a>>2]=f[7416];f[a+4>>2]=f[7417];f[a+8>>2]=f[7418];f[a+12>>2]=f[7419];f[a+16>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+20>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+24>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[a+28>>2]=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[a+32>>2]=g[b+16|0]|g[b+17|0]<<8|(g[b+18|0]<<16|g[b+19|0]<<24);f[a+36>>2]=g[b+20|0]|g[b+21|0]<<8|(g[b+22|0]<<16|g[b+23|0]<<24);f[a+40>>2]=g[b+24|0]|g[b+25|0]<<8|(g[b+26|0]<<16|g[b+27|0]<<24);b=g[b+28|0]|g[b+29|0]<<8|(g[b+30|0]<<16|g[b+31|0]<<24);f[a+48>>2]=0;f[a+44>>2]=b;f[a+52>>2]=0;f[a+56>>2]=g[c|0]|g[c+1|0]<<8|(g[c+2|0]<<16|g[c+3|0]<<24);f[a+60>>2]=g[c+4|0]|g[c+5|0]<<8|(g[c+6|0]<<16|g[c+7|0]<<24)}function ja(a,b){var c=0,d=0;c=A-272|0;A=c;f[c+56>>2]=0;f[c+60>>2]=0;f[c+48>>2]=0;f[c+52>>2]=0;f[c+40>>2]=0;f[c+44>>2]=0;f[c+32>>2]=0;f[c+36>>2]=0;d=g[a+20|0]|g[a+21|0]<<8|(g[a+22|0]<<16|g[a+23|0]<<24);f[c+16>>2]=g[a+16|0]|g[a+17|0]<<8|(g[a+18|0]<<16|g[a+19|0]<<24);f[c+20>>2]=d;d=g[a+28|0]|g[a+29|0]<<8|(g[a+30|0]<<16|g[a+31|0]<<24);f[c+24>>2]=g[a+24|0]|g[a+25|0]<<8|(g[a+26|0]<<16|g[a+27|0]<<24);f[c+28>>2]=d;d=g[a+4|0]|g[a+5|0]<<8|(g[a+6|0]<<16|g[a+7|0]<<24);f[c>>2]=g[a|0]|g[a+1|0]<<8|(g[a+2|0]<<16|g[a+3|0]<<24);f[c+4>>2]=d;d=g[a+12|0]|g[a+13|0]<<8|(g[a+14|0]<<16|g[a+15|0]<<24);f[c+8>>2]=g[a+8|0]|g[a+9|0]<<8|(g[a+10|0]<<16|g[a+11|0]<<24);f[c+12>>2]=d;Z(c+224|0,c,32);Na(c- -64|0,c+224|0);ia(b,c- -64|0);A=c+272|0}function sa(a){var b=0,c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0,l=0,m=0,n=0,o=0,p=0,q=0,r=0,s=0,t=0,u=0,v=0,w=0,x=0,y=0,z=0;c=f[a+28>>2];d=f[a+32>>2];e=f[a+24>>2];g=f[a+20>>2];h=f[a+16>>2];i=f[a+12>>2];j=f[a+8>>2];k=f[a+4>>2];l=f[a>>2];p=S(l,485872621);q=p+541690985|0;r=S(k,q);s=r+796511589|0;t=S(j,s);u=t+935229352|0;v=S(i,u);w=v+20|0;m=S(h,w);n=S(g,m);o=S(e,n);x=S(c,o);y=x+4096|0;z=S(d,y);b=z+ -1|0;f[a+28>>2]=c^b&((c-o|0)+(x<<30)^c);f[a+24>>2]=((e-n|0)+(o<<30)^e)&b^e;f[a+20>>2]=((g-m|0)+(n<<30)^g)&b^g;f[a+16>>2]=((h-w|0)+(m<<30)^h)&b^h;f[a+12>>2]=((i-u|0)+(v<<30)^i)&b^i;f[a+8>>2]=((j-s|0)+(t<<30)^j)&b^j;f[a+4>>2]=((k-q|0)+(r<<30)^k)&b^k;f[a>>2]=(((p<<30)+l|0)+ -485872621^l)&b^l;f[a+32>>2]=((d-y|0)+(z<<16)^d)&b^d}function Za(a,b){var c=0;c=f[a+148>>2]^f[b+148>>2];f[a+144>>2]=f[a+144>>2]^f[b+144>>2];f[a+148>>2]=c;c=f[a+156>>2]^f[b+156>>2];f[a+152>>2]=f[a+152>>2]^f[b+152>>2];f[a+156>>2]=c;c=f[a+164>>2]^f[b+164>>2];f[a+160>>2]=f[a+160>>2]^f[b+160>>2];f[a+164>>2]=c;c=f[a+172>>2]^f[b+172>>2];f[a+168>>2]=f[a+168>>2]^f[b+168>>2];f[a+172>>2]=c;c=f[a+180>>2]^f[b+180>>2];f[a+176>>2]=f[a+176>>2]^f[b+176>>2];f[a+180>>2]=c;c=f[a+188>>2]^f[b+188>>2];f[a+184>>2]=f[a+184>>2]^f[b+184>>2];f[a+188>>2]=c;c=f[a+196>>2]^f[b+196>>2];f[a+192>>2]=f[a+192>>2]^f[b+192>>2];f[a+196>>2]=c;c=f[a+204>>2]^f[b+204>>2];f[a+200>>2]=f[a+200>>2]^f[b+200>>2];f[a+204>>2]=c}function V(a,b,c){var d=0,e=0,g=0,h=0,i=0,j=0,k=0;h=A-128|0;A=h;i=f[a+4>>2];e=i;j=f[a>>2];d=c+j|0;if(d>>>0<c>>>0){e=e+1|0}g=a;f[a>>2]=d;f[a+4>>2]=e;if((e|0)==(i|0)&d>>>0<j>>>0|e>>>0<i>>>0){d=f[a+12>>2];e=f[a+8>>2]+1|0;if(e>>>0<1){d=d+1|0}f[a+8>>2]=e;f[g+12>>2]=d}g=j&127;a:{if(!g){break a}d=128-g|0;if(d>>>0>c>>>0){k=g;break a}e=g;g=a+16|0;P(e+g|0,b,d);la(a,g);b=b+d|0;c=c-d|0}b:{if(!(b&7)){if(c>>>0<=127){break b}while(1){la(a,b);b=b+128|0;c=c+ -128|0;if(c>>>0>127){continue}break}break b}if(c>>>0<128){break b}while(1){la(a,P(h,b,128));b=b+128|0;c=c+ -128|0;if(c>>>0>127){continue}break}}if(c){P((a+k|0)+16|0,b,c)}A=h+128|0}function Zb(a,b){var c=0;f[a+16>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+20>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+24>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);f[a+28>>2]=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);c=g[b+16|0];b=b+16|0;f[a+32>>2]=c|g[b+1|0]<<8|g[b+2|0]<<16|g[b+3|0]<<24;f[a+36>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+40>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24);b=g[b+12|0]|g[b+13|0]<<8|(g[b+14|0]<<16|g[b+15|0]<<24);f[a+12>>2]=1797285236;f[a+8>>2]=2036477234;f[a+4>>2]=857760878;f[a>>2]=1634760805;f[a+44>>2]=b}function gc(a,b,c){var d=0,e=0,g=0,h=0;g=A-176|0;A=g;e=f[a+4>>2];d=f[a>>2];a:{b:{if((e|0)==(d|0)){X(a+8|0,a+208|0,e>>>3|0);break b}e=e-d|0;if(!d|e>>>0>c>>>0){break a}h=d;d=a+208|0;P(h+d|0,b,e);X(a+8|0,d,f[a+4>>2]>>>3|0);b=b+e|0;c=c-e|0}f[a>>2]=0}c:{if(!(b&7)){d=f[a+4>>2];if(c>>>0<d>>>0){break c}e=a+8|0;while(1){X(e,b,d>>>3|0);d=f[a+4>>2];b=d+b|0;c=c-d|0;if(c>>>0>=d>>>0){continue}break}break c}d=f[a+4>>2];if(c>>>0<d>>>0){break c}e=a+8|0;while(1){X(e,P(g,b,d),d>>>3|0);d=f[a+4>>2];b=d+b|0;c=c-d|0;if(c>>>0>=d>>>0){continue}break}}if(c){P((f[a>>2]+a|0)+208|0,b,c);f[a>>2]=f[a>>2]+c}A=g+176|0}function vb(a,b){var c=0,e=0,h=0,i=0,j=0;i=a;while(1){e=j<<2;h=f[e+b>>2];c=0;while(1){d[i|0]=h&15;h=h>>>4|0;i=i+1|0;c=c+1|0;if((c|0)!=7){continue}break}h=f[(e|4)+b>>2]<<2|h;c=0;while(1){e=i;d[e|0]=h&15;h=h>>>4|0;i=e+1|0;c=c+1|0;if((c|0)!=8){continue}break}c=j>>>0<6;j=j+2|0;if(c){continue}break}b=f[b+32>>2];d[e+1|0]=b&15;d[e+4|0]=b>>>12&15;d[e+3|0]=b>>>8&15;d[e+2|0]=(b&240)>>>4;e=g[a|0];i=0;c=0;while(1){b=i+1|0;h=b+a|0;c=c+e|0;e=g[h|0]+(c<<24>>28)|0;d[h|0]=e;h=c&15;c=(c&8)>>>3|0;d[a+i|0]=h-(c<<4);i=b;if((b|0)!=63){continue}break}d[a+63|0]=g[a+63|0]+c}function cc(a,b,c,d,e,f){var g=0,h=0;g=A-688|0;A=g;Ua(g+140|0,d,f);ra(b,c,a,g+624|0);h=a+96|0;ba(g+144|0,h,32);d=Ra(d);a:{if(d){R(g+144|0,1024,1);R(g+144|0,g+624|0,64);break a}R(g+144|0,1026,1);R(g+144|0,a- -64|0,32)}R(g+144|0,g+140|0,4);aa(g+144|0,g- -64|0);Rb(g+560|0,g- -64|0,g+624|0,f);Qb(g+560|0,g- -64|0,g+624|0,f);ba(g+144|0,h,32);b:{if(d){R(g+144|0,1028,1);R(g+144|0,g+624|0,64);break b}R(g+144|0,1030,1);R(g+144|0,a- -64|0,32)}R(g+144|0,g+140|0,4);aa(g+144|0,g);oa(g+624|0);Oa(b,c,g+560|0,g+32|0,e);fa(g+560|0,64);fa(g,64);A=g+688|0}function Ia(a,b,c){var d=0;d=f[c>>2]+f[b>>2]|0;f[a>>2]=d&1073741823;d=f[c+4>>2]+(f[b+4>>2]+(d>>>30|0)|0)|0;f[a+4>>2]=d&1073741823;d=f[c+8>>2]+f[b+8>>2]+(d>>>30)|0;f[a+8>>2]=d&1073741823;d=f[c+12>>2]+f[b+12>>2]+(d>>>30)|0;f[a+12>>2]=d&1073741823;d=f[c+16>>2]+f[b+16>>2]+(d>>>30)|0;f[a+16>>2]=d&1073741823;d=f[c+20>>2]+f[b+20>>2]+(d>>>30)|0;f[a+20>>2]=d&1073741823;d=f[c+24>>2]+f[b+24>>2]+(d>>>30)|0;f[a+24>>2]=d&1073741823;d=f[c+28>>2]+f[b+28>>2]+(d>>>30)|0;f[a+28>>2]=d&1073741823;f[a+32>>2]=f[c+32>>2]+f[b+32>>2]+(d>>>30);sa(a)}function $b(a,b,e){var h=0,j=0,k=0,l=0;h=A+ -64|0;A=h;l=Q(h,0,64);if(!(!b|i[a+228>>2]>e>>>0|(f[a+80>>2]!=0|f[a+84>>2]!=0))){xa(a,f[a+224>>2]);e=a;if(g[a+232|0]){f[e+88>>2]=-1;f[e+92>>2]=-1}f[e+80>>2]=-1;f[e+84>>2]=-1;e=0;h=f[a+224>>2];j=a+96|0;Q(h+j|0,0,128-h|0);wa(a,j);while(1){j=e<<3;h=j+l|0;k=a+j|0;j=f[k+4>>2];k=f[k>>2];d[h|0]=k;d[h+1|0]=k>>>8;d[h+2|0]=k>>>16;d[h+3|0]=k>>>24;d[h+4|0]=j;d[h+5|0]=j>>>8;d[h+6|0]=j>>>16;d[h+7|0]=j>>>24;e=e+1|0;if((e|0)!=8){continue}break}P(b,l,f[a+228>>2]);c[f[7720]](l,0,64)|0}A=l- -64|0}function ga(a,b){var c=0,d=0,e=0,g=0,h=0,i=0,j=0,k=0;c=f[b+4>>2];e=f[b+8>>2];d=f[b+20>>2];g=f[b+24>>2];h=f[b+28>>2];i=f[b>>2];j=f[b+12>>2];b=f[b+16>>2];f[a+20>>2]=b&33554431;f[a+16>>2]=j>>>6;f[a>>2]=i&67108863;f[a+36>>2]=h>>>6&33554431;f[a+28>>2]=((g&524287)<<13|d>>>19)&33554431;k=b;b=d;d=k|0;f[a+24>>2]=((b&33554431)<<7|d>>>25)&67108863;f[a+8>>2]=((e&524287)<<13|c>>>19)&67108863;b=c;c=i;f[a+4>>2]=((b&67108863)<<6|c>>>26)&33554431;f[a+32>>2]=((h&4095)<<20|g>>>12)&67108863;f[a+12>>2]=((j&8191)<<19|e>>>13)&33554431}function Ba(a,b){var c=0;c=f[b+148>>2];f[a+144>>2]=f[b+144>>2];f[a+148>>2]=c;c=f[b+156>>2];f[a+152>>2]=f[b+152>>2];f[a+156>>2]=c;c=f[b+164>>2];f[a+160>>2]=f[b+160>>2];f[a+164>>2]=c;c=f[b+172>>2];f[a+168>>2]=f[b+168>>2];f[a+172>>2]=c;c=f[b+180>>2];f[a+176>>2]=f[b+176>>2];f[a+180>>2]=c;c=f[b+188>>2];f[a+184>>2]=f[b+184>>2];f[a+188>>2]=c;c=f[b+196>>2];f[a+192>>2]=f[b+192>>2];f[a+196>>2]=c;c=f[b+204>>2];f[a+200>>2]=f[b+200>>2];f[a+204>>2]=c}function ec(a,b){var c=0,d=0,e=0,g=0,h=0;c=32;e=A-208|0;A=e;g=f[a+4>>2];d=f[a>>2];a:{b:{if((g|0)==(d|0)){X(a+8|0,0,0);break b}h=!d;d=g-d|0;if(h|d>>>0>32){break a}c=a+8|0;za(e,c);b=P(b,f[a>>2]+e|0,d);X(c,0,0);b=b+d|0;c=32-d|0}f[a>>2]=0}if(c>>>0>i[a+4>>2]){d=a+8|0;while(1){za(e,d);b=P(b,e,f[a+4>>2]);X(d,0,0);g=f[a+4>>2];b=g+b|0;c=c-g|0;if(c>>>0>g>>>0){continue}break}}if(c){za(e,a+8|0);P(b,f[a>>2]+e|0,c);f[a>>2]=f[a>>2]+c}A=e+208|0}function nc(a,b,c){var d=0,e=0,f=0,g=0,h=0;g=c&63;d=g&31;if(32<=g>>>0){e=-1<<d;h=0}else{e=(1<<d)-1&-1>>>32-d|-1<<d;h=-1<<d}h=h&a;d=b&e;f=g&31;if(32<=g>>>0){e=0;g=d>>>f|0}else{e=d>>>f|0;g=((1<<f)-1&d)<<32-f|h>>>f}h=e;d=0-c&63;f=d&31;if(32<=d>>>0){e=0;c=-1>>>f|0}else{e=-1>>>f|0;c=(1<<f)-1<<32-f|-1>>>f}a=c&a;b=b&e;e=d&31;if(32<=d>>>0){c=a<<e;a=0}else{c=(1<<e)-1&a>>>32-e|b<<e;a=a<<e}a=a|g;B=c|h;return a}



function mc(a,b,c){var d=0,e=0,f=0,g=0;g=c&63;f=g;d=f&31;if(32<=f>>>0){d=-1>>>d|0}else{e=-1>>>d|0;d=(1<<d)-1<<32-d|-1>>>d}f=d&a;d=b&e;e=g&31;if(32<=g>>>0){d=f<<e;g=0}else{d=(1<<e)-1&f>>>32-e|d<<e;g=f<<e}f=d;e=0-c&63;d=e;c=d&31;if(32<=d>>>0){d=-1<<c;c=0}else{d=(1<<c)-1&-1>>>32-c|-1<<c;c=-1<<c}a=c&a;d=b&d;b=e&31;if(32<=e>>>0){c=0;a=d>>>b|0}else{c=d>>>b|0;a=((1<<b)-1&d)<<32-b|a>>>b}a=a|g;B=c|f;return a}function $(a){a=Q(a,0,144);f[a+200>>2]=327033209;f[a+204>>2]=1541459225;f[a+192>>2]=-79577749;f[a+196>>2]=528734635;f[a+184>>2]=725511199;f[a+188>>2]=-1694144372;f[a+176>>2]=-1377402159;f[a+180>>2]=1359893119;f[a+168>>2]=1595750129;f[a+172>>2]=-1521486534;f[a+160>>2]=-23791573;f[a+164>>2]=1013904242;f[a+152>>2]=-2067093701;f[a+156>>2]=-1150833019;f[a+144>>2]=-205731576;f[a+148>>2]=1779033703}function $a(a,b,c,e){var f=0,g=0;f=A-768|0;A=f;Ca(f+764|0,b);b=f+624|0;d[Q(b- -64|0,0,60)|0]=128;Ca(b+124|0,1536);P(f+208|0,a,416);R(f+208|0,c,22);R(f+208|0,f+764|0,4);aa(f+208|0,f+624|0);c=f+416|0;b=P(f,c,208);g=a+208|0;f=1;while(1){Ba(b+208|0,a);Da(b+208|0,b+624|0);ma(b+208|0,b+624|0);Ba(c,g);Da(c,b+624|0);ma(c,b+624|0);Za(b,c);f=f+1|0;if((f|0)!=15e3){continue}break}ma(b,e);A=b+768|0}function da(a,b,c){var e=0,h=0,i=0;a:{e=f[a+56>>2];if(e){h=16-e|0;h=h>>>0>c>>>0?c:h;if(h){while(1){d[((e+i|0)+a|0)+60|0]=g[b+i|0];e=f[a+56>>2];i=i+1|0;if((h|0)!=(i|0)){continue}break}}e=e+h|0;f[a+56>>2]=e;if(e>>>0<16){break a}va(a,a+60|0,16);f[a+56>>2]=0;c=c-h|0;b=b+h|0}if(c>>>0>=16){e=c&-16;va(a,b,e);c=c&15;b=b+e|0}if(!c){break a}P((f[a+56>>2]+a|0)+60|0,b,c);f[a+56>>2]=f[a+56>>2]+c}}function ea(a,b){var c=0,d=0,e=0,g=0,h=0;g=A-16|0;A=g;c=f[a+4>>2];d=g;h=f[a>>2];e=f[a+8>>2];f[d>>2]=Aa(e<<3|c>>>29,f[a+12>>2]<<3|e>>>29);f[d+4>>2]=B;e=c<<3;c=h;e=e|c>>>29;f[d+8>>2]=Aa(c<<3,e);f[d+12>>2]=B;d=c&127;V(a,30976,(d>>>0<112?112:240)-d|0);V(a,g,16);d=0;while(1){c=d<<3;h=c+b|0;c=a+c|0;W(h,f[c+144>>2],f[c+148>>2]);d=d+1|0;if((d|0)!=8){continue}break}A=g+16|0}function ba(a,b,c){var e=0;e=A-384|0;A=e;a:{b:{if(c>>>0>=129){$(a);V(a,b,c);ea(a,e+256|0);c=64;break b}if((e+256|0)!=(b|0)){P(e+256|0,b,c)}if(c>>>0>127){break a}}Q((e+256|0)+c|0,0,128-c|0)}c=0;while(1){b=g[(e+256|0)+c|0];d[(e+128|0)+c|0]=b^54;d[c+e|0]=b^92;c=c+1|0;if((c|0)!=128){continue}break}$(a);V(a,e+128|0,128);a=a+208|0;$(a);V(a,e,128);A=e+384|0}function ac(a,b,c){var d=0,e=0,g=0;if(c){d=f[a+224>>2];e=128-d|0;a:{if(e>>>0>=c>>>0){break a}f[a+224>>2]=0;g=d;d=a+96|0;P(g+d|0,b,e);xa(a,128);wa(a,d);b=b+e|0;c=c-e|0;if(c>>>0<129){break a}while(1){xa(a,128);wa(a,b);b=b+128|0;c=c+ -128|0;if(c>>>0>128){continue}break}}P((f[a+224>>2]+a|0)+96|0,b,c);f[a+224>>2]=f[a+224>>2]+c}}function sb(a,b,c,d,e){var f=0;f=A-704|0;A=f;P(f+128|0,c,64);$(f+496|0);R(f+496|0,f+160|0,32);R(f+496|0,a,b);Ka(f+496|0,f- -64|0);Z(f+448|0,f- -64|0,64);Na(f+192|0,f+448|0);ia(e,f+192|0);Ja(f,e,d,a,b);Z(f+400|0,f,64);Z(f+352|0,f+128|0,32);rb(f+400|0,f+400|0,f+352|0);Ia(f+400|0,f+400|0,f+448|0);Ha(e+32|0,f+400|0);A=f+704|0}function Ha(a,b){U(a,f[b>>2]|f[b+4>>2]<<30);U(a+4|0,f[b+8>>2]<<28|f[b+4>>2]>>>2);U(a+8|0,f[b+12>>2]<<26|f[b+8>>2]>>>4);U(a+12|0,f[b+16>>2]<<24|f[b+12>>2]>>>6);U(a+16|0,f[b+20>>2]<<22|f[b+16>>2]>>>8);U(a+20|0,f[b+24>>2]<<20|f[b+20>>2]>>>10);U(a+24|0,f[b+28>>2]<<18|f[b+24>>2]>>>12);U(a+28|0,f[b+32>>2]<<16|f[b+28>>2]>>>14)}function W(a,b,c){var e=0,f=0,g=0,h=0;e=b<<8&16711680|b<<24;f=c<<24|b>>>8;g=f&65280;f=c<<8|b>>>24;e=f&255|g|e;b=((c&255)<<24|b>>>8)&-16777216|((c&16777215)<<8|b>>>24)&16711680|(c>>>8&65280|c>>>24)|h;d[a|0]=b;d[a+1|0]=b>>>8;d[a+2|0]=b>>>16;d[a+3|0]=b>>>24;b=e;d[a+4|0]=b;d[a+5|0]=b>>>8;d[a+6|0]=b>>>16;d[a+7|0]=b>>>24}function dc(a,b){var c=0,d=0,e=0,h=0;Q(a- -64|0,0,176);P(a,30816,64);while(1){c=e<<3;d=c+a|0;c=b+c|0;h=g[c|0]|g[c+1|0]<<8|(g[c+2|0]<<16|g[c+3|0]<<24);c=f[d+4>>2]^(g[c+4|0]|g[c+5|0]<<8|(g[c+6|0]<<16|g[c+7|0]<<24));f[d>>2]=h^f[d>>2];f[d+4>>2]=c;e=e+1|0;if((e|0)!=8){continue}break}f[a+228>>2]=g[b|0]}function Sa(a,b,c,d){var e=0;e=A-112|0;A=e;Wb(e+32|0,a);f[e+8>>2]=0;f[e+12>>2]=0;f[e>>2]=0;f[e+4>>2]=0;da(e+32|0,0,0);da(e+32|0,b,c);a=(c|0)%16|0;if(a){da(e+32|0,e,16-a|0)}f[e+24>>2]=0;f[e+28>>2]=0;da(e+32|0,e+24|0,8);f[e+24>>2]=c;f[e+28>>2]=c>>31;da(e+32|0,e+24|0,8);Vb(e+32|0,d);A=e+112|0}function Yb(a,b,c,d,e,g,h,i){var j=0,k=0;j=A-96|0;A=j;f[j+12>>2]=1;Q(j+32|0,0,64);Va(a,b,0);Ta(a,j+32|0,j+32|0,64);a:{if(!(!h|i)){Sa(j+32|0,c,d,j+16|0);k=-1;if(Xb(j+16|0,g,h)){break a}}Va(a,b,j+12|0);Ta(a,c,e,d);if(!(!h|!i)){Sa(j+32|0,e,d,j+16|0);P(g,j+16|0,h)}k=0}a=k;A=j+96|0;return a}function ma(a,b){W(b,f[a+144>>2],f[a+148>>2]);W(b+8|0,f[a+152>>2],f[a+156>>2]);W(b+16|0,f[a+160>>2],f[a+164>>2]);W(b+24|0,f[a+168>>2],f[a+172>>2]);W(b+32|0,f[a+176>>2],f[a+180>>2]);W(b+40|0,f[a+184>>2],f[a+188>>2]);W(b+48|0,f[a+192>>2],f[a+196>>2]);W(b+56|0,f[a+200>>2],f[a+204>>2])}function Lb(a,b,c,d){var e=0;e=A+ -64|0;A=e;f[e+48>>2]=0;f[e+52>>2]=0;f[e+56>>2]=0;f[e+60>>2]=0;f[e+32>>2]=0;f[e+36>>2]=0;f[e+40>>2]=0;f[e+44>>2]=0;a:{b:{switch(d+ -1|0){case 0:Qa(e+32|0,b);break a;case 1:break b;default:break a}}Pa(e+32|0,b)}ja(e+32|0,e);ib(e,c,a);A=e- -64|0}function Fa(a){var b=0;b=A-96|0;A=b;T(b+48|0,a,5);N(a,b+48|0,a);T(b+48|0,a,10);N(b,b+48|0,a);T(b+48|0,b,20);N(b+48|0,b+48|0,b);T(b+48|0,b+48|0,10);N(a,b+48|0,a);T(b+48|0,a,50);N(b,b+48|0,a);T(b+48|0,b,100);N(b+48|0,b+48|0,b);T(b+48|0,b+48|0,50);N(a,b+48|0,a);A=b+96|0}function xa(a,b){var c=0,d=0,e=0,g=0,h=0,i=0;e=f[a+68>>2];c=e;h=f[a+64>>2];d=h;g=b+d|0;if(g>>>0<d>>>0){c=c+1|0}f[a+64>>2]=g;f[a+68>>2]=c;b=a;i=a;d=f[a+76>>2];c=(c|0)==(e|0)&g>>>0<h>>>0|c>>>0<e>>>0;a=c+f[a+72>>2]|0;if(a>>>0<c>>>0){d=d+1|0}f[i+72>>2]=a;f[b+76>>2]=d}function Va(a,b,c){f[a+48>>2]=c?g[c|0]|g[c+1|0]<<8|(g[c+2|0]<<16|g[c+3|0]<<24):0;f[a+52>>2]=g[b|0]|g[b+1|0]<<8|(g[b+2|0]<<16|g[b+3|0]<<24);f[a+56>>2]=g[b+4|0]|g[b+5|0]<<8|(g[b+6|0]<<16|g[b+7|0]<<24);f[a+60>>2]=g[b+8|0]|g[b+9|0]<<8|(g[b+10|0]<<16|g[b+11|0]<<24)}function Rb(a,b,c,d){var e=0;e=A-32|0;A=e;f[e+24>>2]=0;f[e+28>>2]=0;f[e+16>>2]=0;f[e+20>>2]=0;f[e+8>>2]=0;f[e+12>>2]=0;f[e>>2]=0;f[e+4>>2]=0;a:{b:{switch(d+ -1|0){case 0:Qa(e,b);jb(e,c,a);break a;case 1:break b;default:break a}}Pa(e,b);hc(e,c,a)}A=e+32|0}function qb(a,b,c,d){var e=0,f=0;e=A-512|0;A=e;f=-1;a:{if(g[d+63|0]>31){break a}if(!qa(e+192|0,c)){break a}Ja(e+128|0,d,c,a,b);Z(e+80|0,e+128|0,64);Z(e+32|0,d+32|0,32);pb(e+352|0,e+192|0,e+80|0,e+32|0);ia(e,e+352|0);f=pa(d,e)?0:-1}A=e+512|0;return f}function fc(a){var b=0,c=0;b=f[a>>2];if((b|0)==f[a+4>>2]){X(a+8|0,a+208|0,b>>>3|0);f[a>>2]=0;b=0}f[a>>2]=b+1;c=b;b=a+208|0;d[c+b|0]=6;c=f[a>>2];Q(c+b|0,0,f[a+4>>2]-c|0);c=(f[a+4>>2]+b|0)+ -1|0;d[c|0]=g[c|0]|128;X(a+8|0,b,f[a+4>>2]>>>3|0);f[a>>2]=0}function _a(a,b,c){var d=0,g=0,i=0;d=A-32|0;A=d;g=h[529]|h[530]<<16;i=h[527]|h[528]<<16;e[d+14>>1]=i;e[d+16>>1]=i>>>16;e[d+18>>1]=g;e[d+20>>1]=g>>>16;g=f[263];f[d+8>>2]=f[262];f[d+12>>2]=g;g=f[261];f[d>>2]=f[260];f[d+4>>2]=g;ab(b,c,d,a);A=d+32|0}function ab(a,b,c,d){var e=0,f=0;e=A-480|0;A=e;if(d){ba(e- -64|0,a,b);a=1;while(1){$a(e- -64|0,a,c,e);f=(a<<6)+ -64|0;b=40-f|0;P(d+f|0,e,b>>>0<64?b:64);b=(a|0)==1;a=a+1|0;if(!b){continue}break}A=e+480|0;return}z(29757,29707,363,29769);u()}function Ua(a,b,c){a:{switch(c+ -1|0){case 0:b=b<<8&16711680|b<<24|(b>>>8&65280|b>>>24);d[a|0]=b;d[a+1|0]=b>>>8;d[a+2|0]=b>>>16;d[a+3|0]=b>>>24;return;case 1:d[a|0]=b;d[a+1|0]=b>>>8;d[a+2|0]=b>>>16;d[a+3|0]=b>>>24;break;default:break a}}}function ka(a,b,c,e,f){var g=0,h=0;h=A-192|0;A=h;a:{if(b){g=Q(h,0,131);_a(g+144|0,a,b);a=Q(g,0,136);d[a+130|0]=20;cb(a,g+144|0,g+176|0);fa(g+144|0,40);bb(e,g,c,f);fa(g,131);break a}P(e,c,f)}A=h+192|0}function jc(a,b,c){var d=0,e=0,f=0,g=0,h=0;e=c>>>16|0;d=a>>>16|0;h=l(e,d);f=c&65535;a=a&65535;g=l(f,a);d=(g>>>16|0)+l(d,f)|0;a=(d&65535)+l(a,e)|0;B=h+l(b,c)+(d>>>16)+(a>>>16)|0;return g&65535|a<<16}function ob(a,b){var c=0;c=A-144|0;A=c;T(c+48|0,b,1);T(c,c+48|0,2);N(c+96|0,c,b);N(c+48|0,c+96|0,c+48|0);T(c,c+48|0,1);N(c+96|0,c,c+96|0);Fa(c+96|0);T(c+96|0,c+96|0,2);N(a,c+96|0,b);A=c+144|0}function bc(a,b){var c=0,e=0;c=A+ -64|0;A=c;e=-1;if(b+ -1>>>0<=63){d[c+3|0]=1;d[c+1|0]=256;d[c+2|0]=1;d[c|0]=b;ya(c|4);ya(c|8);ya(c|12);Q(c+16|0,0,48);dc(a,c);e=0}A=c- -64|0;return e}function _(a){var b=0,c=0;b=f[7904];c=a+3&-4;a=b+c|0;a:{if(a>>>0<=b>>>0?(c|0)>=1:0){break a}if(a>>>0>C()<<16>>>0){if(!(x(a|0)|0)){break a}}f[7904]=a;return b}f[7776]=48;return-1}function tb(a,b){var c=0;c=A-144|0;A=c;T(c+96|0,b,1);T(c+48|0,c+96|0,2);N(c,c+48|0,b);N(c+96|0,c,c+96|0);T(c+48|0,c+96|0,1);N(c,c+48|0,c);Fa(c);T(c,c,5);N(a,c,c+96|0);A=c+144|0}function Ab(a,b,c,d,e,f,g,h){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;g=g|0;h=h|0;var i=0;i=A+ -64|0;A=i;i=Q(i,0,64);Zb(Q(i,0,64),a);a=Yb(i,b,c,d,e,f,g,h);A=i- -64|0;return a|0}function ia(a,b){var c=0;c=A-176|0;A=c;tb(c+32|0,b+80|0);N(c+128|0,b,c+32|0);N(c+80|0,b+40|0,c+32|0);ca(a,c+80|0);ca(c,c+128|0);d[a+31|0]=g[a+31|0]^g[c|0]<<7;A=c+176|0}function _b(a,b,c,d){var e=0,f=0;e=A-240|0;A=e;f=-1;a:{if(!a|!c&(d|0)!=0|b+ -1>>>0>63){break a}if((bc(e,b)|0)<0){break a}ac(e,c,d);$b(e,a,b);f=0}A=e+240|0;return f}function ib(a,b,c){var e=0;e=A-480|0;A=e;a:{if(!qa(e+160|0,a)){break a}if(!qa(e,b)){break a}hb(e+320|0,e+160|0,e);ia(c,e+320|0);d[c+31|0]=g[c+31|0]^128}A=e+480|0}function Pa(a,b){var c=0,e=0,f=0;while(1){f=c;c=b+e|0;d[a+e|0]=f|g[c|0]<<3;c=g[c|0]>>>5|0;e=e+1|0;if((e|0)!=28){continue}break}d[a+28|0]=g[b+27|0]>>>5}function hb(a,b,c){var d=0,e=0;d=A-160|0;A=d;gb(d,b,c);b=d+120|0;N(a,d,b);c=d+40|0;e=d+80|0;N(a+40|0,c,e);N(a+80|0,e,b);N(a+120|0,d,c);A=d+160|0}function La(a,b){var c=0,d=0,e=0;c=A-160|0;A=c;na(c,b);b=c+120|0;N(a,c,b);d=c+40|0;e=c+80|0;N(a+40|0,d,e);N(a+80|0,e,b);N(a+120|0,c,d);A=c+160|0}function Xb(a,b,c){var d=0,e=0;if((c|0)<1){return 0}while(1){d=g[b|0]^g[a|0]|d;b=b+1|0;a=a+1|0;e=e+1|0;if((e|0)!=(c|0)){continue}break}return d}function Qb(a,b,c,d){a:{switch(d+ -1|0){case 0:Ob(a+32|0,b+32|0,c+32|0);return;case 1:Nb(a+32|0,b+32|0,c+32|0);break;default:break a}}}function za(a,b){var c=0,d=0;c=25;while(1){d=f[b+4>>2];f[a>>2]=f[b>>2];f[a+4>>2]=d;a=a+8|0;b=b+8|0;c=c+ -1|0;if(c){continue}break}}function Ya(a,b){var c=0;c=16;while(1){f[a>>2]=Aa(f[b>>2],f[b+4>>2]);f[a+4>>2]=B;a=a+8|0;b=b+8|0;c=c+ -1|0;if(c){continue}break}}function Nb(a,b,c){var e=0,f=0;while(1){f=g[c+e|0]+(g[b+e|0]+f|0)|0;d[a+e|0]=f;f=f>>>0>255;e=e+1|0;if((e|0)!=32){continue}break}}function Cb(a,b,c){a=a|0;b=b|0;c=c|0;var d=0;d=A-208|0;A=d;d=Q(d,0,208);f[Q(d,0,344)+4>>2]=136;gc(d,a,b);fc(d);ec(d,c);A=d+208|0}function pa(a,b){var c=0,d=0;c=32;while(1){d=g[b|0]^g[a|0]|d;b=b+1|0;a=a+1|0;c=c+ -1|0;if(c){continue}break}return d+ -1>>>8&1}function hc(a,b,c){var e=0,f=0;while(1){f=g[b+e|0]+(g[a+e|0]+f|0)|0;d[c+e|0]=f;f=f>>>8|0;e=e+1|0;if((e|0)!=32){continue}break}}function ta(a,b){var c=0,d=0;c=A-160|0;A=c;na(c,b);b=c+120|0;N(a,c,b);d=c+80|0;N(a+40|0,c+40|0,d);N(a+80|0,d,b);A=c+160|0}function Ca(a,b){b=b<<8&16711680|b<<24|(b>>>8&65280|b>>>24);d[a|0]=b;d[a+1|0]=b>>>8;d[a+2|0]=b>>>16;d[a+3|0]=b>>>24}function ub(a,b,c,d,e,f){var g=0;g=A-96|0;A=g;ra(b,c,a,g+32|0);ja(g+32|0,g);sb(d,e,g+32|0,g,f);oa(g+32|0);A=g+96|0}function jb(a,b,c){var d=0;d=A-96|0;A=d;Z(d+48|0,a,32);Z(d,b,32);Ia(d+48|0,d+48|0,d);Ha(c,d+48|0);A=d+96|0}function Pb(a,b,c,d,e){var f=0,g=0;f=A+ -64|0;A=f;g=1;if(!fb(c,f)){Oa(a,b,f,d,e);g=0}A=f- -64|0;return g}function Ob(a,b,c){var e=0;while(1){d[a+e|0]=g[c+e|0]+g[b+e|0];e=e+1|0;if((e|0)!=32){continue}break}}function Ja(a,b,c,d,e){var f=0;f=A-208|0;A=f;$(f);R(f,b,32);R(f,c,32);R(f,d,e);Ka(f,a);A=f+208|0}function Qa(a,b){var c=0;while(1){d[a+c|0]=g[b+c|0]<<3;c=c+1|0;if((c|0)!=32){continue}break}}function lc(a,b){var c=0,d=0;c=b&31;d=(-1>>>c&a)<<c;c=a;a=0-b&31;return d|(c&-1<<a)>>>a}function fb(a,b){eb(b,a);d[b|0]=g[b|0]&248;a=g[b+31|0];d[b+31|0]=a&63|64;return a>>>5&1}function Eb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;return Mb(a,b,c,d,e,f)|0}function Kb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;ub(c,a,b,d,e,f)}function Gb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;cc(c,a,b,d,e,f)}function Fb(a,b,c,d,e,f){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;f=f|0;db(a,b,c,d,e,f)}function Hb(a,b,c,d,e){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;return Pb(a,b,c,d,e)|0}function eb(a,b){var c=0;c=A-208|0;A=c;$(c);V(c,b,32);ea(c,a);A=c+208|0}function U(a,b){d[a|0]=b;d[a+1|0]=b>>>8;d[a+2|0]=b>>>16;d[a+3|0]=b>>>24}function Bb(a,b,c,d,e){a=a|0;b=b|0;c=c|0;d=d|0;e=e|0;ka(a,b,c,d,e)}function Jb(a,b,c,d){a=a|0;b=b|0;c=c|0;d=d|0;return qb(a,b,c,d)|0}function Db(a,b,c,d){a=a|0;b=b|0;c=c|0;d=d|0;return _b(c,d,a,b)|0}function Xa(a){return a<<8&16711680|a<<24|(a>>>8&65280|a>>>24)}function ya(a){d[a|0]=0;d[a+1|0]=0;d[a+2|0]=0;d[a+3|0]=0}function aa(a,b){ea(a,b);a=a+208|0;V(a,b,64);ea(a,b)}function yb(a,b,c){a=a|0;b=b|0;c=c|0;ba(a,b,c)}function xb(a,b,c){a=a|0;b=b|0;c=c|0;R(a,b,c)}function Aa(a,b){b=Xa(b);B=Xa(a);return b}function Wa(a,b,c){a=mc(a,b,c);return a}function M(a,b,c){a=nc(a,b,c);return a}function wb(a,b){a=a|0;b=b|0;aa(a,b)}function Ib(a,b){a=a|0;b=b|0;ja(a,b)}function ra(a,b,c,d){ka(a,b,c,d,64)}function kc(a,b,c){return jc(a,b,c)}function Ra(a){return a&-2147483648}function Sb(a){a=a|0;return u()|0}function S(a,b){return a-b>>>31|0}function O(a,b){return lc(a,b)}function Da(a,b){V(a,b,128)}function R(a,b,c){V(a,b,c)}function fa(a,b){Q(a,0,b)}function zb(){return 416}function Ka(a,b){ea(a,b)}function oa(a){fa(a,64)}function ic(){}
// EMSCRIPTEN_END_FUNCS
c[1]=Q;function C(){return buffer.byteLength/65536|0}return{"d":ic,"e":Kb,"f":Jb,"g":Ib,"h":Hb,"i":Gb,"j":Fb,"k":Eb,"l":Db,"m":Cb,"n":Bb,"o":Ab,"p":zb,"q":yb,"r":xb,"s":wb,"t":Ub,"u":Tb,"v":Sb}}var D=new Uint8Array(wasmMemory.buffer);for(var E=new Uint8Array(123),F=25;F>=0;--F){E[48+F]=52+F;E[65+F]=F;E[97+F]=26+F}E[43]=62;E[47]=63;function G(uint8Array,offset,b64){var H,I,F=0,J=offset,K=b64.length,L=offset+(K*3>>2)-(b64[K-2]=="=")-(b64[K-1]=="=");for(;F<K;F+=4){H=E[b64.charCodeAt(F+1)];I=E[b64.charCodeAt(F+2)];uint8Array[J++]=E[b64.charCodeAt(F)]<<2|H>>4;if(J<L)uint8Array[J++]=H<<4|I>>2;if(J<L)uint8Array[J++]=I<<6|E[b64.charCodeAt(F+3)]}}G(D,1026,"AgABAAM=");G(D,1040,"ZW5jcnlwdGVkIHdhbGxldCBzYWx0");G(D,1072,"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");G(D,25728,"sKAOAtLJhgGdGI8Af2k1AGAMvQCn1/sBnkyAAmll4QEd/AQAkgyu");G(D,25776,"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");G(D,29664,"ZXhwYW5kIDMyLWJ5dGUga2V4cGFuZCAxNi1ieXRlIGtpdGVyYXRpb25zAHZlbmRvci9jYml0cy9jcnlwdG9uaXRlX2NiaXRzL2NyeXB0b25pdGVfcGJrZGYyLmMAb3V0ICYmIG5vdXQAcGJrZGYyX3NoYTUxMg==");G(D,29792,"Iq4o15gvikLNZe8jkUQ3cS87TezP+8C1vNuJgaXbtek4tUjzW8JWORnQBbbxEfFZm08Zr6SCP5IYgW3a1V4cq0ICA6OYqgfYvm9wRQFbgxKMsuROvoUxJOK0/9XDfQxVb4l78nRdvnKxlhY7/rHegDUSxyWnBtyblCZpz3Txm8HSSvGewWmb5OMlTziGR77vtdWMi8adwQ9lnKx3zKEMJHUCK1lvLOktg+SmbqqEdErU+0G93KmwXLVTEYPaiPl2q99m7lJRPpgQMrQtbcYxqD8h+5jIJwOw5A7vvsd/Wb/Cj6g98wvgxiWnCpNHkafVb4ID4FFjygZwbg4KZykpFPwv0kaFCrcnJskmXDghGy7tKsRa/G0sTd+zlZ0TDThT3mOvi1RzCmWosnc8uwpqduau7UcuycKBOzWCFIUscpJkA/FMoei/ogEwQrxLZhqokZf40HCLS8IwvlQGo1FsxxhS79YZ6JLREKllVSQGmdYqIHFXhTUO9LjRuzJwoGoQyNDSuBbBpBlTq0FRCGw3Hpnrjt9Md0gnqEib4bW8sDRjWsnFswwcOcuKQeNKqthOc+Njd0/KnFujuLLW828uaPyy713ugo90YC8XQ29jpXhyq/ChFHjIhOw5ZBoIAseMKB5jI/r/vpDpvYLe62xQpBV5xrL3o/m+K1Ny4/J4ccacYSbqzj4nygfCwCHHuIbRHuvgzdZ92up40W7uf0999bpvF3KqZ/AGppjIosV9YwquDfm+BJg/ERtHHBM1C3EbhH0EI/V32yiTJMdAe6vKMry+yRUKvp48TA0QnMRnHUO2Qj7LvtTFTCp+ZfycKX9Z7PrWOqtvy18XWEdKjBlEbAoAAAAHAAAACwAAABEAAAASAAAAAwAAAAUAAAAQAAAACAAAABUAAAAYAAAABAAAAA8AAAAXAAAAEwAAAA0AAAAMAAAAAgAAABQAAAAOAAAAFgAAAAkAAAAGAAAAAQAAAAEAAAADAAAABgAAAAoAAAAPAAAAFQAAABwAAAAkAAAALQAAADcAAAACAAAADgAAABsAAAApAAAAOAAAAAgAAAAZAAAAKwAAAD4AAAASAAAAJwAAAD0AAAAUAAAALAAAAAEAAAAAAAAAgoAAAAAAAACKgAAAAAAAgACAAIAAAACAi4AAAAAAAAABAACAAAAAAIGAAIAAAACACYAAAAAAAICKAAAAAAAAAIgAAAAAAAAACYAAgAAAAAAKAACAAAAAAIuAAIAAAAAAiwAAAAAAAICJgAAAAAAAgAOAAAAAAACAAoAAAAAAAICAAAAAAAAAgAqAAAAAAAAACgAAgAAAAICBgACAAAAAgICAAAAAAACAAQAAgAAAAAAIgACAAAAAgAjJvPNn5glqO6fKhIWuZ7sr+JT+cvNuPPE2HV869U+l0YLmrX9SDlEfbD4rjGgFm2u9Qfur2YMfeSF+ExnN4FsBAAAAa2V5X2xlbiA9PSAxMjggfHwga2V5X2xlbiA9PSAyNTYAdmVuZG9yL2NiaXRzL2NoYWNoYXBvbHkvY2hhY2hhcG9seS5jAGNoYWNoYXBvbHlfaW5pdA==");G(D,30976,"gA==");return b({"Int8Array":Int8Array,"Int16Array":Int16Array,"Int32Array":Int32Array,"Uint8Array":Uint8Array,"Uint16Array":Uint16Array,"Uint32Array":Uint32Array,"Float32Array":Float32Array,"Float64Array":Float64Array,"NaN":NaN,"Infinity":Infinity,"Math":Math},asmLibraryArg,wasmMemory.buffer)}


// EMSCRIPTEN_END_ASM




)(asmLibraryArg,wasmMemory,wasmTable)},instantiate:function(binary,info){return{then:function(ok){var module=new WebAssembly.Module(binary);ok({"instance":new WebAssembly.Instance(module)})}}},RuntimeError:Error};wasmBinary=[];if(typeof WebAssembly!=="object"){abort("no native wasm support detected")}var wasmMemory;var wasmTable=new WebAssembly.Table({"initial":2,"maximum":2,"element":"anyfunc"});var ABORT=false;var EXITSTATUS=0;function assert(condition,text){if(!condition){abort("Assertion failed: "+text)}}var UTF8Decoder=typeof TextDecoder!=="undefined"?new TextDecoder("utf8"):undefined;function UTF8ArrayToString(heap,idx,maxBytesToRead){var endIdx=idx+maxBytesToRead;var endPtr=idx;while(heap[endPtr]&&!(endPtr>=endIdx))++endPtr;if(endPtr-idx>16&&heap.subarray&&UTF8Decoder){return UTF8Decoder.decode(heap.subarray(idx,endPtr))}else{var str="";while(idx<endPtr){var u0=heap[idx++];if(!(u0&128)){str+=String.fromCharCode(u0);continue}var u1=heap[idx++]&63;if((u0&224)==192){str+=String.fromCharCode((u0&31)<<6|u1);continue}var u2=heap[idx++]&63;if((u0&240)==224){u0=(u0&15)<<12|u1<<6|u2}else{u0=(u0&7)<<18|u1<<12|u2<<6|heap[idx++]&63}if(u0<65536){str+=String.fromCharCode(u0)}else{var ch=u0-65536;str+=String.fromCharCode(55296|ch>>10,56320|ch&1023)}}}return str}function UTF8ToString(ptr,maxBytesToRead){return ptr?UTF8ArrayToString(HEAPU8,ptr,maxBytesToRead):""}var WASM_PAGE_SIZE=65536;var buffer,HEAP8,HEAPU8,HEAP16,HEAPU16,HEAP32,HEAPU32,HEAPF32,HEAPF64;function updateGlobalBufferAndViews(buf){buffer=buf;Module["HEAP8"]=HEAP8=new Int8Array(buf);Module["HEAP16"]=HEAP16=new Int16Array(buf);Module["HEAP32"]=HEAP32=new Int32Array(buf);Module["HEAPU8"]=HEAPU8=new Uint8Array(buf);Module["HEAPU16"]=HEAPU16=new Uint16Array(buf);Module["HEAPU32"]=HEAPU32=new Uint32Array(buf);Module["HEAPF32"]=HEAPF32=new Float32Array(buf);Module["HEAPF64"]=HEAPF64=new Float64Array(buf)}var DYNAMIC_BASE=5274656,DYNAMICTOP_PTR=31616;var INITIAL_INITIAL_MEMORY=Module["INITIAL_MEMORY"]||16777216;if(Module["wasmMemory"]){wasmMemory=Module["wasmMemory"]}else{wasmMemory=new WebAssembly.Memory({"initial":INITIAL_INITIAL_MEMORY/WASM_PAGE_SIZE,"maximum":INITIAL_INITIAL_MEMORY/WASM_PAGE_SIZE})}if(wasmMemory){buffer=wasmMemory.buffer}INITIAL_INITIAL_MEMORY=buffer.byteLength;updateGlobalBufferAndViews(buffer);HEAP32[DYNAMICTOP_PTR>>2]=DYNAMIC_BASE;function callRuntimeCallbacks(callbacks){while(callbacks.length>0){var callback=callbacks.shift();if(typeof callback=="function"){callback(Module);continue}var func=callback.func;if(typeof func==="number"){if(callback.arg===undefined){Module["dynCall_v"](func)}else{Module["dynCall_vi"](func,callback.arg)}}else{func(callback.arg===undefined?null:callback.arg)}}}var __ATPRERUN__=[];var __ATINIT__=[];var __ATMAIN__=[];var __ATPOSTRUN__=[];var runtimeInitialized=false;function preRun(){if(Module["preRun"]){if(typeof Module["preRun"]=="function")Module["preRun"]=[Module["preRun"]];while(Module["preRun"].length){addOnPreRun(Module["preRun"].shift())}}callRuntimeCallbacks(__ATPRERUN__)}function initRuntime(){runtimeInitialized=true;callRuntimeCallbacks(__ATINIT__)}function preMain(){callRuntimeCallbacks(__ATMAIN__)}function postRun(){if(Module["postRun"]){if(typeof Module["postRun"]=="function")Module["postRun"]=[Module["postRun"]];while(Module["postRun"].length){addOnPostRun(Module["postRun"].shift())}}callRuntimeCallbacks(__ATPOSTRUN__)}function addOnPreRun(cb){__ATPRERUN__.unshift(cb)}function addOnPostRun(cb){__ATPOSTRUN__.unshift(cb)}var runDependencies=0;var runDependencyWatcher=null;var dependenciesFulfilled=null;function addRunDependency(id){runDependencies++;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}}function removeRunDependency(id){runDependencies--;if(Module["monitorRunDependencies"]){Module["monitorRunDependencies"](runDependencies)}if(runDependencies==0){if(runDependencyWatcher!==null){clearInterval(runDependencyWatcher);runDependencyWatcher=null}if(dependenciesFulfilled){var callback=dependenciesFulfilled;dependenciesFulfilled=null;callback()}}}Module["preloadedImages"]={};Module["preloadedAudios"]={};function abort(what){if(Module["onAbort"]){Module["onAbort"](what)}what+="";err(what);ABORT=true;EXITSTATUS=1;what="abort("+what+"). Build with -s ASSERTIONS=1 for more info.";var e=new WebAssembly.RuntimeError(what);throw e}function hasPrefix(str,prefix){return String.prototype.startsWith?str.startsWith(prefix):str.indexOf(prefix)===0}var dataURIPrefix="data:application/octet-stream;base64,";function isDataURI(filename){return hasPrefix(filename,dataURIPrefix)}var fileURIPrefix="file://";function isFileURI(filename){return hasPrefix(filename,fileURIPrefix)}var wasmBinaryFile="lib.wasm";if(!isDataURI(wasmBinaryFile)){wasmBinaryFile=locateFile(wasmBinaryFile)}function getBinary(){try{if(wasmBinary){return new Uint8Array(wasmBinary)}var binary=tryParseAsDataURI(wasmBinaryFile);if(binary){return binary}if(readBinary){return readBinary(wasmBinaryFile)}else{throw"both async and sync fetching of the wasm failed"}}catch(err){abort(err)}}function getBinaryPromise(){if(!wasmBinary&&(ENVIRONMENT_IS_WEB||ENVIRONMENT_IS_WORKER)&&typeof fetch==="function"&&!isFileURI(wasmBinaryFile)){return fetch(wasmBinaryFile,{credentials:"same-origin"}).then(function(response){if(!response["ok"]){throw"failed to load wasm binary file at '"+wasmBinaryFile+"'"}return response["arrayBuffer"]()}).catch(function(){return getBinary()})}return Promise.resolve().then(getBinary)}function createWasm(){var info={"a":asmLibraryArg};function receiveInstance(instance,module){var exports=instance.exports;Module["asm"]=exports;removeRunDependency("wasm-instantiate")}addRunDependency("wasm-instantiate");function receiveInstantiatedSource(output){receiveInstance(output["instance"])}function instantiateArrayBuffer(receiver){return getBinaryPromise().then(function(binary){return WebAssembly.instantiate(binary,info)}).then(receiver,function(reason){err("failed to asynchronously prepare wasm: "+reason);abort(reason)})}function instantiateAsync(){if(!wasmBinary&&typeof WebAssembly.instantiateStreaming==="function"&&!isDataURI(wasmBinaryFile)&&!isFileURI(wasmBinaryFile)&&typeof fetch==="function"){fetch(wasmBinaryFile,{credentials:"same-origin"}).then(function(response){var result=WebAssembly.instantiateStreaming(response,info);return result.then(receiveInstantiatedSource,function(reason){err("wasm streaming compile failed: "+reason);err("falling back to ArrayBuffer instantiation");return instantiateArrayBuffer(receiveInstantiatedSource)})})}else{return instantiateArrayBuffer(receiveInstantiatedSource)}}if(Module["instantiateWasm"]){try{var exports=Module["instantiateWasm"](info,receiveInstance);return exports}catch(e){err("Module.instantiateWasm callback failed with error: "+e);return false}}instantiateAsync();return{}}__ATINIT__.push({func:function(){___wasm_call_ctors()}});function ___assert_fail(condition,filename,line,func){abort("Assertion failed: "+UTF8ToString(condition)+", at: "+[filename?UTF8ToString(filename):"unknown filename",line,func?UTF8ToString(func):"unknown function"])}function _emscripten_memcpy_big(dest,src,num){HEAPU8.copyWithin(dest,src,src+num)}function abortOnCannotGrowMemory(requestedSize){abort("OOM")}function _emscripten_resize_heap(requestedSize){requestedSize=requestedSize>>>0;abortOnCannotGrowMemory(requestedSize)}var ASSERTIONS=false;function intArrayToString(array){var ret=[];for(var i=0;i<array.length;i++){var chr=array[i];if(chr>255){if(ASSERTIONS){assert(false,"Character code "+chr+" ("+String.fromCharCode(chr)+")  at offset "+i+" not in 0x00-0xFF.")}chr&=255}ret.push(String.fromCharCode(chr))}return ret.join("")}var decodeBase64=typeof atob==="function"?atob:function(input){var keyStr="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";var output="";var chr1,chr2,chr3;var enc1,enc2,enc3,enc4;var i=0;input=input.replace(/[^A-Za-z0-9\+\/\=]/g,"");do{enc1=keyStr.indexOf(input.charAt(i++));enc2=keyStr.indexOf(input.charAt(i++));enc3=keyStr.indexOf(input.charAt(i++));enc4=keyStr.indexOf(input.charAt(i++));chr1=enc1<<2|enc2>>4;chr2=(enc2&15)<<4|enc3>>2;chr3=(enc3&3)<<6|enc4;output=output+String.fromCharCode(chr1);if(enc3!==64){output=output+String.fromCharCode(chr2)}if(enc4!==64){output=output+String.fromCharCode(chr3)}}while(i<input.length);return output};function intArrayFromBase64(s){if(typeof ENVIRONMENT_IS_NODE==="boolean"&&ENVIRONMENT_IS_NODE){var buf;try{buf=Buffer.from(s,"base64")}catch(_){buf=new Buffer(s,"base64")}return new Uint8Array(buf["buffer"],buf["byteOffset"],buf["byteLength"])}try{var decoded=decodeBase64(s);var bytes=new Uint8Array(decoded.length);for(var i=0;i<decoded.length;++i){bytes[i]=decoded.charCodeAt(i)}return bytes}catch(_){throw new Error("Converting base64 string to bytes failed.")}}function tryParseAsDataURI(filename){if(!isDataURI(filename)){return}return intArrayFromBase64(filename.slice(dataURIPrefix.length))}var asmLibraryArg={"c":___assert_fail,"b":_emscripten_memcpy_big,"a":_emscripten_resize_heap,"memory":wasmMemory,"table":wasmTable};var asm=createWasm();var ___wasm_call_ctors=Module["___wasm_call_ctors"]=function(){return(___wasm_call_ctors=Module["___wasm_call_ctors"]=Module["asm"]["d"]).apply(null,arguments)};var _emscripten_sign=Module["_emscripten_sign"]=function(){return(_emscripten_sign=Module["_emscripten_sign"]=Module["asm"]["e"]).apply(null,arguments)};var _emscripten_verify=Module["_emscripten_verify"]=function(){return(_emscripten_verify=Module["_emscripten_verify"]=Module["asm"]["f"]).apply(null,arguments)};var _emscripten_to_public=Module["_emscripten_to_public"]=function(){return(_emscripten_to_public=Module["_emscripten_to_public"]=Module["asm"]["g"]).apply(null,arguments)};var _emscripten_wallet_secret_from_seed=Module["_emscripten_wallet_secret_from_seed"]=function(){return(_emscripten_wallet_secret_from_seed=Module["_emscripten_wallet_secret_from_seed"]=Module["asm"]["h"]).apply(null,arguments)};var _emscripten_derive_private=Module["_emscripten_derive_private"]=function(){return(_emscripten_derive_private=Module["_emscripten_derive_private"]=Module["asm"]["i"]).apply(null,arguments)};var _emscripten_wallet_change_pass=Module["_emscripten_wallet_change_pass"]=function(){return(_emscripten_wallet_change_pass=Module["_emscripten_wallet_change_pass"]=Module["asm"]["j"]).apply(null,arguments)};var _emscripten_derive_public=Module["_emscripten_derive_public"]=function(){return(_emscripten_derive_public=Module["_emscripten_derive_public"]=Module["asm"]["k"]).apply(null,arguments)};var _emscripten_blake2b=Module["_emscripten_blake2b"]=function(){return(_emscripten_blake2b=Module["_emscripten_blake2b"]=Module["asm"]["l"]).apply(null,arguments)};var _emscripten_sha3_256=Module["_emscripten_sha3_256"]=function(){return(_emscripten_sha3_256=Module["_emscripten_sha3_256"]=Module["asm"]["m"]).apply(null,arguments)};var _emscripten_cardano_memory_combine=Module["_emscripten_cardano_memory_combine"]=function(){return(_emscripten_cardano_memory_combine=Module["_emscripten_cardano_memory_combine"]=Module["asm"]["n"]).apply(null,arguments)};var _emscripten_chacha20poly1305_enc=Module["_emscripten_chacha20poly1305_enc"]=function(){return(_emscripten_chacha20poly1305_enc=Module["_emscripten_chacha20poly1305_enc"]=Module["asm"]["o"]).apply(null,arguments)};var _emscripten_size_of_hmac_sha512_ctx=Module["_emscripten_size_of_hmac_sha512_ctx"]=function(){return(_emscripten_size_of_hmac_sha512_ctx=Module["_emscripten_size_of_hmac_sha512_ctx"]=Module["asm"]["p"]).apply(null,arguments)};var _emscripten_hmac_sha512_init=Module["_emscripten_hmac_sha512_init"]=function(){return(_emscripten_hmac_sha512_init=Module["_emscripten_hmac_sha512_init"]=Module["asm"]["q"]).apply(null,arguments)};var _emscripten_hmac_sha512_update=Module["_emscripten_hmac_sha512_update"]=function(){return(_emscripten_hmac_sha512_update=Module["_emscripten_hmac_sha512_update"]=Module["asm"]["r"]).apply(null,arguments)};var _emscripten_hmac_sha512_final=Module["_emscripten_hmac_sha512_final"]=function(){return(_emscripten_hmac_sha512_final=Module["_emscripten_hmac_sha512_final"]=Module["asm"]["s"]).apply(null,arguments)};var _malloc=Module["_malloc"]=function(){return(_malloc=Module["_malloc"]=Module["asm"]["t"]).apply(null,arguments)};var _free=Module["_free"]=function(){return(_free=Module["_free"]=Module["asm"]["u"]).apply(null,arguments)};var __growWasmMemory=Module["__growWasmMemory"]=function(){return(__growWasmMemory=Module["__growWasmMemory"]=Module["asm"]["v"]).apply(null,arguments)};var calledRun;dependenciesFulfilled=function runCaller(){if(!calledRun)run();if(!calledRun)dependenciesFulfilled=runCaller};function run(args){args=args||arguments_;if(runDependencies>0){return}preRun();if(runDependencies>0)return;function doRun(){if(calledRun)return;calledRun=true;Module["calledRun"]=true;if(ABORT)return;initRuntime();preMain();if(Module["onRuntimeInitialized"])Module["onRuntimeInitialized"]();postRun()}if(Module["setStatus"]){Module["setStatus"]("Running...");setTimeout(function(){setTimeout(function(){Module["setStatus"]("")},1);doRun()},1)}else{doRun()}}Module["run"]=run;if(Module["preInit"]){if(typeof Module["preInit"]=="function")Module["preInit"]=[Module["preInit"]];while(Module["preInit"].length>0){Module["preInit"].pop()()}}noExitRuntime=true;run();
if (typeof module !== "undefined") {  module["exports"] = Module; }
}).call(this,require('_process'),require("buffer").Buffer,"/")
},{"_process":53,"buffer":44,"fs":41,"path":51}],7:[function(require,module,exports){
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

},{"./wordlists/chinese_simplified.json":9,"./wordlists/chinese_traditional.json":10,"./wordlists/english.json":11,"./wordlists/french.json":12,"./wordlists/italian.json":13,"./wordlists/japanese.json":14,"./wordlists/korean.json":15,"./wordlists/spanish.json":16}],8:[function(require,module,exports){
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
},{"./_wordlists":7,"buffer":44,"create-hash":18,"pbkdf2":23,"randombytes":28}],9:[function(require,module,exports){
module.exports=[
    "的",
    "一",
    "是",
    "在",
    "不",
    "了",
    "有",
    "和",
    "人",
    "这",
    "中",
    "大",
    "为",
    "上",
    "个",
    "国",
    "我",
    "以",
    "要",
    "他",
    "时",
    "来",
    "用",
    "们",
    "生",
    "到",
    "作",
    "地",
    "于",
    "出",
    "就",
    "分",
    "对",
    "成",
    "会",
    "可",
    "主",
    "发",
    "年",
    "动",
    "同",
    "工",
    "也",
    "能",
    "下",
    "过",
    "子",
    "说",
    "产",
    "种",
    "面",
    "而",
    "方",
    "后",
    "多",
    "定",
    "行",
    "学",
    "法",
    "所",
    "民",
    "得",
    "经",
    "十",
    "三",
    "之",
    "进",
    "着",
    "等",
    "部",
    "度",
    "家",
    "电",
    "力",
    "里",
    "如",
    "水",
    "化",
    "高",
    "自",
    "二",
    "理",
    "起",
    "小",
    "物",
    "现",
    "实",
    "加",
    "量",
    "都",
    "两",
    "体",
    "制",
    "机",
    "当",
    "使",
    "点",
    "从",
    "业",
    "本",
    "去",
    "把",
    "性",
    "好",
    "应",
    "开",
    "它",
    "合",
    "还",
    "因",
    "由",
    "其",
    "些",
    "然",
    "前",
    "外",
    "天",
    "政",
    "四",
    "日",
    "那",
    "社",
    "义",
    "事",
    "平",
    "形",
    "相",
    "全",
    "表",
    "间",
    "样",
    "与",
    "关",
    "各",
    "重",
    "新",
    "线",
    "内",
    "数",
    "正",
    "心",
    "反",
    "你",
    "明",
    "看",
    "原",
    "又",
    "么",
    "利",
    "比",
    "或",
    "但",
    "质",
    "气",
    "第",
    "向",
    "道",
    "命",
    "此",
    "变",
    "条",
    "只",
    "没",
    "结",
    "解",
    "问",
    "意",
    "建",
    "月",
    "公",
    "无",
    "系",
    "军",
    "很",
    "情",
    "者",
    "最",
    "立",
    "代",
    "想",
    "已",
    "通",
    "并",
    "提",
    "直",
    "题",
    "党",
    "程",
    "展",
    "五",
    "果",
    "料",
    "象",
    "员",
    "革",
    "位",
    "入",
    "常",
    "文",
    "总",
    "次",
    "品",
    "式",
    "活",
    "设",
    "及",
    "管",
    "特",
    "件",
    "长",
    "求",
    "老",
    "头",
    "基",
    "资",
    "边",
    "流",
    "路",
    "级",
    "少",
    "图",
    "山",
    "统",
    "接",
    "知",
    "较",
    "将",
    "组",
    "见",
    "计",
    "别",
    "她",
    "手",
    "角",
    "期",
    "根",
    "论",
    "运",
    "农",
    "指",
    "几",
    "九",
    "区",
    "强",
    "放",
    "决",
    "西",
    "被",
    "干",
    "做",
    "必",
    "战",
    "先",
    "回",
    "则",
    "任",
    "取",
    "据",
    "处",
    "队",
    "南",
    "给",
    "色",
    "光",
    "门",
    "即",
    "保",
    "治",
    "北",
    "造",
    "百",
    "规",
    "热",
    "领",
    "七",
    "海",
    "口",
    "东",
    "导",
    "器",
    "压",
    "志",
    "世",
    "金",
    "增",
    "争",
    "济",
    "阶",
    "油",
    "思",
    "术",
    "极",
    "交",
    "受",
    "联",
    "什",
    "认",
    "六",
    "共",
    "权",
    "收",
    "证",
    "改",
    "清",
    "美",
    "再",
    "采",
    "转",
    "更",
    "单",
    "风",
    "切",
    "打",
    "白",
    "教",
    "速",
    "花",
    "带",
    "安",
    "场",
    "身",
    "车",
    "例",
    "真",
    "务",
    "具",
    "万",
    "每",
    "目",
    "至",
    "达",
    "走",
    "积",
    "示",
    "议",
    "声",
    "报",
    "斗",
    "完",
    "类",
    "八",
    "离",
    "华",
    "名",
    "确",
    "才",
    "科",
    "张",
    "信",
    "马",
    "节",
    "话",
    "米",
    "整",
    "空",
    "元",
    "况",
    "今",
    "集",
    "温",
    "传",
    "土",
    "许",
    "步",
    "群",
    "广",
    "石",
    "记",
    "需",
    "段",
    "研",
    "界",
    "拉",
    "林",
    "律",
    "叫",
    "且",
    "究",
    "观",
    "越",
    "织",
    "装",
    "影",
    "算",
    "低",
    "持",
    "音",
    "众",
    "书",
    "布",
    "复",
    "容",
    "儿",
    "须",
    "际",
    "商",
    "非",
    "验",
    "连",
    "断",
    "深",
    "难",
    "近",
    "矿",
    "千",
    "周",
    "委",
    "素",
    "技",
    "备",
    "半",
    "办",
    "青",
    "省",
    "列",
    "习",
    "响",
    "约",
    "支",
    "般",
    "史",
    "感",
    "劳",
    "便",
    "团",
    "往",
    "酸",
    "历",
    "市",
    "克",
    "何",
    "除",
    "消",
    "构",
    "府",
    "称",
    "太",
    "准",
    "精",
    "值",
    "号",
    "率",
    "族",
    "维",
    "划",
    "选",
    "标",
    "写",
    "存",
    "候",
    "毛",
    "亲",
    "快",
    "效",
    "斯",
    "院",
    "查",
    "江",
    "型",
    "眼",
    "王",
    "按",
    "格",
    "养",
    "易",
    "置",
    "派",
    "层",
    "片",
    "始",
    "却",
    "专",
    "状",
    "育",
    "厂",
    "京",
    "识",
    "适",
    "属",
    "圆",
    "包",
    "火",
    "住",
    "调",
    "满",
    "县",
    "局",
    "照",
    "参",
    "红",
    "细",
    "引",
    "听",
    "该",
    "铁",
    "价",
    "严",
    "首",
    "底",
    "液",
    "官",
    "德",
    "随",
    "病",
    "苏",
    "失",
    "尔",
    "死",
    "讲",
    "配",
    "女",
    "黄",
    "推",
    "显",
    "谈",
    "罪",
    "神",
    "艺",
    "呢",
    "席",
    "含",
    "企",
    "望",
    "密",
    "批",
    "营",
    "项",
    "防",
    "举",
    "球",
    "英",
    "氧",
    "势",
    "告",
    "李",
    "台",
    "落",
    "木",
    "帮",
    "轮",
    "破",
    "亚",
    "师",
    "围",
    "注",
    "远",
    "字",
    "材",
    "排",
    "供",
    "河",
    "态",
    "封",
    "另",
    "施",
    "减",
    "树",
    "溶",
    "怎",
    "止",
    "案",
    "言",
    "士",
    "均",
    "武",
    "固",
    "叶",
    "鱼",
    "波",
    "视",
    "仅",
    "费",
    "紧",
    "爱",
    "左",
    "章",
    "早",
    "朝",
    "害",
    "续",
    "轻",
    "服",
    "试",
    "食",
    "充",
    "兵",
    "源",
    "判",
    "护",
    "司",
    "足",
    "某",
    "练",
    "差",
    "致",
    "板",
    "田",
    "降",
    "黑",
    "犯",
    "负",
    "击",
    "范",
    "继",
    "兴",
    "似",
    "余",
    "坚",
    "曲",
    "输",
    "修",
    "故",
    "城",
    "夫",
    "够",
    "送",
    "笔",
    "船",
    "占",
    "右",
    "财",
    "吃",
    "富",
    "春",
    "职",
    "觉",
    "汉",
    "画",
    "功",
    "巴",
    "跟",
    "虽",
    "杂",
    "飞",
    "检",
    "吸",
    "助",
    "升",
    "阳",
    "互",
    "初",
    "创",
    "抗",
    "考",
    "投",
    "坏",
    "策",
    "古",
    "径",
    "换",
    "未",
    "跑",
    "留",
    "钢",
    "曾",
    "端",
    "责",
    "站",
    "简",
    "述",
    "钱",
    "副",
    "尽",
    "帝",
    "射",
    "草",
    "冲",
    "承",
    "独",
    "令",
    "限",
    "阿",
    "宣",
    "环",
    "双",
    "请",
    "超",
    "微",
    "让",
    "控",
    "州",
    "良",
    "轴",
    "找",
    "否",
    "纪",
    "益",
    "依",
    "优",
    "顶",
    "础",
    "载",
    "倒",
    "房",
    "突",
    "坐",
    "粉",
    "敌",
    "略",
    "客",
    "袁",
    "冷",
    "胜",
    "绝",
    "析",
    "块",
    "剂",
    "测",
    "丝",
    "协",
    "诉",
    "念",
    "陈",
    "仍",
    "罗",
    "盐",
    "友",
    "洋",
    "错",
    "苦",
    "夜",
    "刑",
    "移",
    "频",
    "逐",
    "靠",
    "混",
    "母",
    "短",
    "皮",
    "终",
    "聚",
    "汽",
    "村",
    "云",
    "哪",
    "既",
    "距",
    "卫",
    "停",
    "烈",
    "央",
    "察",
    "烧",
    "迅",
    "境",
    "若",
    "印",
    "洲",
    "刻",
    "括",
    "激",
    "孔",
    "搞",
    "甚",
    "室",
    "待",
    "核",
    "校",
    "散",
    "侵",
    "吧",
    "甲",
    "游",
    "久",
    "菜",
    "味",
    "旧",
    "模",
    "湖",
    "货",
    "损",
    "预",
    "阻",
    "毫",
    "普",
    "稳",
    "乙",
    "妈",
    "植",
    "息",
    "扩",
    "银",
    "语",
    "挥",
    "酒",
    "守",
    "拿",
    "序",
    "纸",
    "医",
    "缺",
    "雨",
    "吗",
    "针",
    "刘",
    "啊",
    "急",
    "唱",
    "误",
    "训",
    "愿",
    "审",
    "附",
    "获",
    "茶",
    "鲜",
    "粮",
    "斤",
    "孩",
    "脱",
    "硫",
    "肥",
    "善",
    "龙",
    "演",
    "父",
    "渐",
    "血",
    "欢",
    "械",
    "掌",
    "歌",
    "沙",
    "刚",
    "攻",
    "谓",
    "盾",
    "讨",
    "晚",
    "粒",
    "乱",
    "燃",
    "矛",
    "乎",
    "杀",
    "药",
    "宁",
    "鲁",
    "贵",
    "钟",
    "煤",
    "读",
    "班",
    "伯",
    "香",
    "介",
    "迫",
    "句",
    "丰",
    "培",
    "握",
    "兰",
    "担",
    "弦",
    "蛋",
    "沉",
    "假",
    "穿",
    "执",
    "答",
    "乐",
    "谁",
    "顺",
    "烟",
    "缩",
    "征",
    "脸",
    "喜",
    "松",
    "脚",
    "困",
    "异",
    "免",
    "背",
    "星",
    "福",
    "买",
    "染",
    "井",
    "概",
    "慢",
    "怕",
    "磁",
    "倍",
    "祖",
    "皇",
    "促",
    "静",
    "补",
    "评",
    "翻",
    "肉",
    "践",
    "尼",
    "衣",
    "宽",
    "扬",
    "棉",
    "希",
    "伤",
    "操",
    "垂",
    "秋",
    "宜",
    "氢",
    "套",
    "督",
    "振",
    "架",
    "亮",
    "末",
    "宪",
    "庆",
    "编",
    "牛",
    "触",
    "映",
    "雷",
    "销",
    "诗",
    "座",
    "居",
    "抓",
    "裂",
    "胞",
    "呼",
    "娘",
    "景",
    "威",
    "绿",
    "晶",
    "厚",
    "盟",
    "衡",
    "鸡",
    "孙",
    "延",
    "危",
    "胶",
    "屋",
    "乡",
    "临",
    "陆",
    "顾",
    "掉",
    "呀",
    "灯",
    "岁",
    "措",
    "束",
    "耐",
    "剧",
    "玉",
    "赵",
    "跳",
    "哥",
    "季",
    "课",
    "凯",
    "胡",
    "额",
    "款",
    "绍",
    "卷",
    "齐",
    "伟",
    "蒸",
    "殖",
    "永",
    "宗",
    "苗",
    "川",
    "炉",
    "岩",
    "弱",
    "零",
    "杨",
    "奏",
    "沿",
    "露",
    "杆",
    "探",
    "滑",
    "镇",
    "饭",
    "浓",
    "航",
    "怀",
    "赶",
    "库",
    "夺",
    "伊",
    "灵",
    "税",
    "途",
    "灭",
    "赛",
    "归",
    "召",
    "鼓",
    "播",
    "盘",
    "裁",
    "险",
    "康",
    "唯",
    "录",
    "菌",
    "纯",
    "借",
    "糖",
    "盖",
    "横",
    "符",
    "私",
    "努",
    "堂",
    "域",
    "枪",
    "润",
    "幅",
    "哈",
    "竟",
    "熟",
    "虫",
    "泽",
    "脑",
    "壤",
    "碳",
    "欧",
    "遍",
    "侧",
    "寨",
    "敢",
    "彻",
    "虑",
    "斜",
    "薄",
    "庭",
    "纳",
    "弹",
    "饲",
    "伸",
    "折",
    "麦",
    "湿",
    "暗",
    "荷",
    "瓦",
    "塞",
    "床",
    "筑",
    "恶",
    "户",
    "访",
    "塔",
    "奇",
    "透",
    "梁",
    "刀",
    "旋",
    "迹",
    "卡",
    "氯",
    "遇",
    "份",
    "毒",
    "泥",
    "退",
    "洗",
    "摆",
    "灰",
    "彩",
    "卖",
    "耗",
    "夏",
    "择",
    "忙",
    "铜",
    "献",
    "硬",
    "予",
    "繁",
    "圈",
    "雪",
    "函",
    "亦",
    "抽",
    "篇",
    "阵",
    "阴",
    "丁",
    "尺",
    "追",
    "堆",
    "雄",
    "迎",
    "泛",
    "爸",
    "楼",
    "避",
    "谋",
    "吨",
    "野",
    "猪",
    "旗",
    "累",
    "偏",
    "典",
    "馆",
    "索",
    "秦",
    "脂",
    "潮",
    "爷",
    "豆",
    "忽",
    "托",
    "惊",
    "塑",
    "遗",
    "愈",
    "朱",
    "替",
    "纤",
    "粗",
    "倾",
    "尚",
    "痛",
    "楚",
    "谢",
    "奋",
    "购",
    "磨",
    "君",
    "池",
    "旁",
    "碎",
    "骨",
    "监",
    "捕",
    "弟",
    "暴",
    "割",
    "贯",
    "殊",
    "释",
    "词",
    "亡",
    "壁",
    "顿",
    "宝",
    "午",
    "尘",
    "闻",
    "揭",
    "炮",
    "残",
    "冬",
    "桥",
    "妇",
    "警",
    "综",
    "招",
    "吴",
    "付",
    "浮",
    "遭",
    "徐",
    "您",
    "摇",
    "谷",
    "赞",
    "箱",
    "隔",
    "订",
    "男",
    "吹",
    "园",
    "纷",
    "唐",
    "败",
    "宋",
    "玻",
    "巨",
    "耕",
    "坦",
    "荣",
    "闭",
    "湾",
    "键",
    "凡",
    "驻",
    "锅",
    "救",
    "恩",
    "剥",
    "凝",
    "碱",
    "齿",
    "截",
    "炼",
    "麻",
    "纺",
    "禁",
    "废",
    "盛",
    "版",
    "缓",
    "净",
    "睛",
    "昌",
    "婚",
    "涉",
    "筒",
    "嘴",
    "插",
    "岸",
    "朗",
    "庄",
    "街",
    "藏",
    "姑",
    "贸",
    "腐",
    "奴",
    "啦",
    "惯",
    "乘",
    "伙",
    "恢",
    "匀",
    "纱",
    "扎",
    "辩",
    "耳",
    "彪",
    "臣",
    "亿",
    "璃",
    "抵",
    "脉",
    "秀",
    "萨",
    "俄",
    "网",
    "舞",
    "店",
    "喷",
    "纵",
    "寸",
    "汗",
    "挂",
    "洪",
    "贺",
    "闪",
    "柬",
    "爆",
    "烯",
    "津",
    "稻",
    "墙",
    "软",
    "勇",
    "像",
    "滚",
    "厘",
    "蒙",
    "芳",
    "肯",
    "坡",
    "柱",
    "荡",
    "腿",
    "仪",
    "旅",
    "尾",
    "轧",
    "冰",
    "贡",
    "登",
    "黎",
    "削",
    "钻",
    "勒",
    "逃",
    "障",
    "氨",
    "郭",
    "峰",
    "币",
    "港",
    "伏",
    "轨",
    "亩",
    "毕",
    "擦",
    "莫",
    "刺",
    "浪",
    "秘",
    "援",
    "株",
    "健",
    "售",
    "股",
    "岛",
    "甘",
    "泡",
    "睡",
    "童",
    "铸",
    "汤",
    "阀",
    "休",
    "汇",
    "舍",
    "牧",
    "绕",
    "炸",
    "哲",
    "磷",
    "绩",
    "朋",
    "淡",
    "尖",
    "启",
    "陷",
    "柴",
    "呈",
    "徒",
    "颜",
    "泪",
    "稍",
    "忘",
    "泵",
    "蓝",
    "拖",
    "洞",
    "授",
    "镜",
    "辛",
    "壮",
    "锋",
    "贫",
    "虚",
    "弯",
    "摩",
    "泰",
    "幼",
    "廷",
    "尊",
    "窗",
    "纲",
    "弄",
    "隶",
    "疑",
    "氏",
    "宫",
    "姐",
    "震",
    "瑞",
    "怪",
    "尤",
    "琴",
    "循",
    "描",
    "膜",
    "违",
    "夹",
    "腰",
    "缘",
    "珠",
    "穷",
    "森",
    "枝",
    "竹",
    "沟",
    "催",
    "绳",
    "忆",
    "邦",
    "剩",
    "幸",
    "浆",
    "栏",
    "拥",
    "牙",
    "贮",
    "礼",
    "滤",
    "钠",
    "纹",
    "罢",
    "拍",
    "咱",
    "喊",
    "袖",
    "埃",
    "勤",
    "罚",
    "焦",
    "潜",
    "伍",
    "墨",
    "欲",
    "缝",
    "姓",
    "刊",
    "饱",
    "仿",
    "奖",
    "铝",
    "鬼",
    "丽",
    "跨",
    "默",
    "挖",
    "链",
    "扫",
    "喝",
    "袋",
    "炭",
    "污",
    "幕",
    "诸",
    "弧",
    "励",
    "梅",
    "奶",
    "洁",
    "灾",
    "舟",
    "鉴",
    "苯",
    "讼",
    "抱",
    "毁",
    "懂",
    "寒",
    "智",
    "埔",
    "寄",
    "届",
    "跃",
    "渡",
    "挑",
    "丹",
    "艰",
    "贝",
    "碰",
    "拔",
    "爹",
    "戴",
    "码",
    "梦",
    "芽",
    "熔",
    "赤",
    "渔",
    "哭",
    "敬",
    "颗",
    "奔",
    "铅",
    "仲",
    "虎",
    "稀",
    "妹",
    "乏",
    "珍",
    "申",
    "桌",
    "遵",
    "允",
    "隆",
    "螺",
    "仓",
    "魏",
    "锐",
    "晓",
    "氮",
    "兼",
    "隐",
    "碍",
    "赫",
    "拨",
    "忠",
    "肃",
    "缸",
    "牵",
    "抢",
    "博",
    "巧",
    "壳",
    "兄",
    "杜",
    "讯",
    "诚",
    "碧",
    "祥",
    "柯",
    "页",
    "巡",
    "矩",
    "悲",
    "灌",
    "龄",
    "伦",
    "票",
    "寻",
    "桂",
    "铺",
    "圣",
    "恐",
    "恰",
    "郑",
    "趣",
    "抬",
    "荒",
    "腾",
    "贴",
    "柔",
    "滴",
    "猛",
    "阔",
    "辆",
    "妻",
    "填",
    "撤",
    "储",
    "签",
    "闹",
    "扰",
    "紫",
    "砂",
    "递",
    "戏",
    "吊",
    "陶",
    "伐",
    "喂",
    "疗",
    "瓶",
    "婆",
    "抚",
    "臂",
    "摸",
    "忍",
    "虾",
    "蜡",
    "邻",
    "胸",
    "巩",
    "挤",
    "偶",
    "弃",
    "槽",
    "劲",
    "乳",
    "邓",
    "吉",
    "仁",
    "烂",
    "砖",
    "租",
    "乌",
    "舰",
    "伴",
    "瓜",
    "浅",
    "丙",
    "暂",
    "燥",
    "橡",
    "柳",
    "迷",
    "暖",
    "牌",
    "秧",
    "胆",
    "详",
    "簧",
    "踏",
    "瓷",
    "谱",
    "呆",
    "宾",
    "糊",
    "洛",
    "辉",
    "愤",
    "竞",
    "隙",
    "怒",
    "粘",
    "乃",
    "绪",
    "肩",
    "籍",
    "敏",
    "涂",
    "熙",
    "皆",
    "侦",
    "悬",
    "掘",
    "享",
    "纠",
    "醒",
    "狂",
    "锁",
    "淀",
    "恨",
    "牲",
    "霸",
    "爬",
    "赏",
    "逆",
    "玩",
    "陵",
    "祝",
    "秒",
    "浙",
    "貌",
    "役",
    "彼",
    "悉",
    "鸭",
    "趋",
    "凤",
    "晨",
    "畜",
    "辈",
    "秩",
    "卵",
    "署",
    "梯",
    "炎",
    "滩",
    "棋",
    "驱",
    "筛",
    "峡",
    "冒",
    "啥",
    "寿",
    "译",
    "浸",
    "泉",
    "帽",
    "迟",
    "硅",
    "疆",
    "贷",
    "漏",
    "稿",
    "冠",
    "嫩",
    "胁",
    "芯",
    "牢",
    "叛",
    "蚀",
    "奥",
    "鸣",
    "岭",
    "羊",
    "凭",
    "串",
    "塘",
    "绘",
    "酵",
    "融",
    "盆",
    "锡",
    "庙",
    "筹",
    "冻",
    "辅",
    "摄",
    "袭",
    "筋",
    "拒",
    "僚",
    "旱",
    "钾",
    "鸟",
    "漆",
    "沈",
    "眉",
    "疏",
    "添",
    "棒",
    "穗",
    "硝",
    "韩",
    "逼",
    "扭",
    "侨",
    "凉",
    "挺",
    "碗",
    "栽",
    "炒",
    "杯",
    "患",
    "馏",
    "劝",
    "豪",
    "辽",
    "勃",
    "鸿",
    "旦",
    "吏",
    "拜",
    "狗",
    "埋",
    "辊",
    "掩",
    "饮",
    "搬",
    "骂",
    "辞",
    "勾",
    "扣",
    "估",
    "蒋",
    "绒",
    "雾",
    "丈",
    "朵",
    "姆",
    "拟",
    "宇",
    "辑",
    "陕",
    "雕",
    "偿",
    "蓄",
    "崇",
    "剪",
    "倡",
    "厅",
    "咬",
    "驶",
    "薯",
    "刷",
    "斥",
    "番",
    "赋",
    "奉",
    "佛",
    "浇",
    "漫",
    "曼",
    "扇",
    "钙",
    "桃",
    "扶",
    "仔",
    "返",
    "俗",
    "亏",
    "腔",
    "鞋",
    "棱",
    "覆",
    "框",
    "悄",
    "叔",
    "撞",
    "骗",
    "勘",
    "旺",
    "沸",
    "孤",
    "吐",
    "孟",
    "渠",
    "屈",
    "疾",
    "妙",
    "惜",
    "仰",
    "狠",
    "胀",
    "谐",
    "抛",
    "霉",
    "桑",
    "岗",
    "嘛",
    "衰",
    "盗",
    "渗",
    "脏",
    "赖",
    "涌",
    "甜",
    "曹",
    "阅",
    "肌",
    "哩",
    "厉",
    "烃",
    "纬",
    "毅",
    "昨",
    "伪",
    "症",
    "煮",
    "叹",
    "钉",
    "搭",
    "茎",
    "笼",
    "酷",
    "偷",
    "弓",
    "锥",
    "恒",
    "杰",
    "坑",
    "鼻",
    "翼",
    "纶",
    "叙",
    "狱",
    "逮",
    "罐",
    "络",
    "棚",
    "抑",
    "膨",
    "蔬",
    "寺",
    "骤",
    "穆",
    "冶",
    "枯",
    "册",
    "尸",
    "凸",
    "绅",
    "坯",
    "牺",
    "焰",
    "轰",
    "欣",
    "晋",
    "瘦",
    "御",
    "锭",
    "锦",
    "丧",
    "旬",
    "锻",
    "垄",
    "搜",
    "扑",
    "邀",
    "亭",
    "酯",
    "迈",
    "舒",
    "脆",
    "酶",
    "闲",
    "忧",
    "酚",
    "顽",
    "羽",
    "涨",
    "卸",
    "仗",
    "陪",
    "辟",
    "惩",
    "杭",
    "姚",
    "肚",
    "捉",
    "飘",
    "漂",
    "昆",
    "欺",
    "吾",
    "郎",
    "烷",
    "汁",
    "呵",
    "饰",
    "萧",
    "雅",
    "邮",
    "迁",
    "燕",
    "撒",
    "姻",
    "赴",
    "宴",
    "烦",
    "债",
    "帐",
    "斑",
    "铃",
    "旨",
    "醇",
    "董",
    "饼",
    "雏",
    "姿",
    "拌",
    "傅",
    "腹",
    "妥",
    "揉",
    "贤",
    "拆",
    "歪",
    "葡",
    "胺",
    "丢",
    "浩",
    "徽",
    "昂",
    "垫",
    "挡",
    "览",
    "贪",
    "慰",
    "缴",
    "汪",
    "慌",
    "冯",
    "诺",
    "姜",
    "谊",
    "凶",
    "劣",
    "诬",
    "耀",
    "昏",
    "躺",
    "盈",
    "骑",
    "乔",
    "溪",
    "丛",
    "卢",
    "抹",
    "闷",
    "咨",
    "刮",
    "驾",
    "缆",
    "悟",
    "摘",
    "铒",
    "掷",
    "颇",
    "幻",
    "柄",
    "惠",
    "惨",
    "佳",
    "仇",
    "腊",
    "窝",
    "涤",
    "剑",
    "瞧",
    "堡",
    "泼",
    "葱",
    "罩",
    "霍",
    "捞",
    "胎",
    "苍",
    "滨",
    "俩",
    "捅",
    "湘",
    "砍",
    "霞",
    "邵",
    "萄",
    "疯",
    "淮",
    "遂",
    "熊",
    "粪",
    "烘",
    "宿",
    "档",
    "戈",
    "驳",
    "嫂",
    "裕",
    "徙",
    "箭",
    "捐",
    "肠",
    "撑",
    "晒",
    "辨",
    "殿",
    "莲",
    "摊",
    "搅",
    "酱",
    "屏",
    "疫",
    "哀",
    "蔡",
    "堵",
    "沫",
    "皱",
    "畅",
    "叠",
    "阁",
    "莱",
    "敲",
    "辖",
    "钩",
    "痕",
    "坝",
    "巷",
    "饿",
    "祸",
    "丘",
    "玄",
    "溜",
    "曰",
    "逻",
    "彭",
    "尝",
    "卿",
    "妨",
    "艇",
    "吞",
    "韦",
    "怨",
    "矮",
    "歇"
]

},{}],10:[function(require,module,exports){
module.exports=[
    "的",
    "一",
    "是",
    "在",
    "不",
    "了",
    "有",
    "和",
    "人",
    "這",
    "中",
    "大",
    "為",
    "上",
    "個",
    "國",
    "我",
    "以",
    "要",
    "他",
    "時",
    "來",
    "用",
    "們",
    "生",
    "到",
    "作",
    "地",
    "於",
    "出",
    "就",
    "分",
    "對",
    "成",
    "會",
    "可",
    "主",
    "發",
    "年",
    "動",
    "同",
    "工",
    "也",
    "能",
    "下",
    "過",
    "子",
    "說",
    "產",
    "種",
    "面",
    "而",
    "方",
    "後",
    "多",
    "定",
    "行",
    "學",
    "法",
    "所",
    "民",
    "得",
    "經",
    "十",
    "三",
    "之",
    "進",
    "著",
    "等",
    "部",
    "度",
    "家",
    "電",
    "力",
    "裡",
    "如",
    "水",
    "化",
    "高",
    "自",
    "二",
    "理",
    "起",
    "小",
    "物",
    "現",
    "實",
    "加",
    "量",
    "都",
    "兩",
    "體",
    "制",
    "機",
    "當",
    "使",
    "點",
    "從",
    "業",
    "本",
    "去",
    "把",
    "性",
    "好",
    "應",
    "開",
    "它",
    "合",
    "還",
    "因",
    "由",
    "其",
    "些",
    "然",
    "前",
    "外",
    "天",
    "政",
    "四",
    "日",
    "那",
    "社",
    "義",
    "事",
    "平",
    "形",
    "相",
    "全",
    "表",
    "間",
    "樣",
    "與",
    "關",
    "各",
    "重",
    "新",
    "線",
    "內",
    "數",
    "正",
    "心",
    "反",
    "你",
    "明",
    "看",
    "原",
    "又",
    "麼",
    "利",
    "比",
    "或",
    "但",
    "質",
    "氣",
    "第",
    "向",
    "道",
    "命",
    "此",
    "變",
    "條",
    "只",
    "沒",
    "結",
    "解",
    "問",
    "意",
    "建",
    "月",
    "公",
    "無",
    "系",
    "軍",
    "很",
    "情",
    "者",
    "最",
    "立",
    "代",
    "想",
    "已",
    "通",
    "並",
    "提",
    "直",
    "題",
    "黨",
    "程",
    "展",
    "五",
    "果",
    "料",
    "象",
    "員",
    "革",
    "位",
    "入",
    "常",
    "文",
    "總",
    "次",
    "品",
    "式",
    "活",
    "設",
    "及",
    "管",
    "特",
    "件",
    "長",
    "求",
    "老",
    "頭",
    "基",
    "資",
    "邊",
    "流",
    "路",
    "級",
    "少",
    "圖",
    "山",
    "統",
    "接",
    "知",
    "較",
    "將",
    "組",
    "見",
    "計",
    "別",
    "她",
    "手",
    "角",
    "期",
    "根",
    "論",
    "運",
    "農",
    "指",
    "幾",
    "九",
    "區",
    "強",
    "放",
    "決",
    "西",
    "被",
    "幹",
    "做",
    "必",
    "戰",
    "先",
    "回",
    "則",
    "任",
    "取",
    "據",
    "處",
    "隊",
    "南",
    "給",
    "色",
    "光",
    "門",
    "即",
    "保",
    "治",
    "北",
    "造",
    "百",
    "規",
    "熱",
    "領",
    "七",
    "海",
    "口",
    "東",
    "導",
    "器",
    "壓",
    "志",
    "世",
    "金",
    "增",
    "爭",
    "濟",
    "階",
    "油",
    "思",
    "術",
    "極",
    "交",
    "受",
    "聯",
    "什",
    "認",
    "六",
    "共",
    "權",
    "收",
    "證",
    "改",
    "清",
    "美",
    "再",
    "採",
    "轉",
    "更",
    "單",
    "風",
    "切",
    "打",
    "白",
    "教",
    "速",
    "花",
    "帶",
    "安",
    "場",
    "身",
    "車",
    "例",
    "真",
    "務",
    "具",
    "萬",
    "每",
    "目",
    "至",
    "達",
    "走",
    "積",
    "示",
    "議",
    "聲",
    "報",
    "鬥",
    "完",
    "類",
    "八",
    "離",
    "華",
    "名",
    "確",
    "才",
    "科",
    "張",
    "信",
    "馬",
    "節",
    "話",
    "米",
    "整",
    "空",
    "元",
    "況",
    "今",
    "集",
    "溫",
    "傳",
    "土",
    "許",
    "步",
    "群",
    "廣",
    "石",
    "記",
    "需",
    "段",
    "研",
    "界",
    "拉",
    "林",
    "律",
    "叫",
    "且",
    "究",
    "觀",
    "越",
    "織",
    "裝",
    "影",
    "算",
    "低",
    "持",
    "音",
    "眾",
    "書",
    "布",
    "复",
    "容",
    "兒",
    "須",
    "際",
    "商",
    "非",
    "驗",
    "連",
    "斷",
    "深",
    "難",
    "近",
    "礦",
    "千",
    "週",
    "委",
    "素",
    "技",
    "備",
    "半",
    "辦",
    "青",
    "省",
    "列",
    "習",
    "響",
    "約",
    "支",
    "般",
    "史",
    "感",
    "勞",
    "便",
    "團",
    "往",
    "酸",
    "歷",
    "市",
    "克",
    "何",
    "除",
    "消",
    "構",
    "府",
    "稱",
    "太",
    "準",
    "精",
    "值",
    "號",
    "率",
    "族",
    "維",
    "劃",
    "選",
    "標",
    "寫",
    "存",
    "候",
    "毛",
    "親",
    "快",
    "效",
    "斯",
    "院",
    "查",
    "江",
    "型",
    "眼",
    "王",
    "按",
    "格",
    "養",
    "易",
    "置",
    "派",
    "層",
    "片",
    "始",
    "卻",
    "專",
    "狀",
    "育",
    "廠",
    "京",
    "識",
    "適",
    "屬",
    "圓",
    "包",
    "火",
    "住",
    "調",
    "滿",
    "縣",
    "局",
    "照",
    "參",
    "紅",
    "細",
    "引",
    "聽",
    "該",
    "鐵",
    "價",
    "嚴",
    "首",
    "底",
    "液",
    "官",
    "德",
    "隨",
    "病",
    "蘇",
    "失",
    "爾",
    "死",
    "講",
    "配",
    "女",
    "黃",
    "推",
    "顯",
    "談",
    "罪",
    "神",
    "藝",
    "呢",
    "席",
    "含",
    "企",
    "望",
    "密",
    "批",
    "營",
    "項",
    "防",
    "舉",
    "球",
    "英",
    "氧",
    "勢",
    "告",
    "李",
    "台",
    "落",
    "木",
    "幫",
    "輪",
    "破",
    "亞",
    "師",
    "圍",
    "注",
    "遠",
    "字",
    "材",
    "排",
    "供",
    "河",
    "態",
    "封",
    "另",
    "施",
    "減",
    "樹",
    "溶",
    "怎",
    "止",
    "案",
    "言",
    "士",
    "均",
    "武",
    "固",
    "葉",
    "魚",
    "波",
    "視",
    "僅",
    "費",
    "緊",
    "愛",
    "左",
    "章",
    "早",
    "朝",
    "害",
    "續",
    "輕",
    "服",
    "試",
    "食",
    "充",
    "兵",
    "源",
    "判",
    "護",
    "司",
    "足",
    "某",
    "練",
    "差",
    "致",
    "板",
    "田",
    "降",
    "黑",
    "犯",
    "負",
    "擊",
    "范",
    "繼",
    "興",
    "似",
    "餘",
    "堅",
    "曲",
    "輸",
    "修",
    "故",
    "城",
    "夫",
    "夠",
    "送",
    "筆",
    "船",
    "佔",
    "右",
    "財",
    "吃",
    "富",
    "春",
    "職",
    "覺",
    "漢",
    "畫",
    "功",
    "巴",
    "跟",
    "雖",
    "雜",
    "飛",
    "檢",
    "吸",
    "助",
    "昇",
    "陽",
    "互",
    "初",
    "創",
    "抗",
    "考",
    "投",
    "壞",
    "策",
    "古",
    "徑",
    "換",
    "未",
    "跑",
    "留",
    "鋼",
    "曾",
    "端",
    "責",
    "站",
    "簡",
    "述",
    "錢",
    "副",
    "盡",
    "帝",
    "射",
    "草",
    "衝",
    "承",
    "獨",
    "令",
    "限",
    "阿",
    "宣",
    "環",
    "雙",
    "請",
    "超",
    "微",
    "讓",
    "控",
    "州",
    "良",
    "軸",
    "找",
    "否",
    "紀",
    "益",
    "依",
    "優",
    "頂",
    "礎",
    "載",
    "倒",
    "房",
    "突",
    "坐",
    "粉",
    "敵",
    "略",
    "客",
    "袁",
    "冷",
    "勝",
    "絕",
    "析",
    "塊",
    "劑",
    "測",
    "絲",
    "協",
    "訴",
    "念",
    "陳",
    "仍",
    "羅",
    "鹽",
    "友",
    "洋",
    "錯",
    "苦",
    "夜",
    "刑",
    "移",
    "頻",
    "逐",
    "靠",
    "混",
    "母",
    "短",
    "皮",
    "終",
    "聚",
    "汽",
    "村",
    "雲",
    "哪",
    "既",
    "距",
    "衛",
    "停",
    "烈",
    "央",
    "察",
    "燒",
    "迅",
    "境",
    "若",
    "印",
    "洲",
    "刻",
    "括",
    "激",
    "孔",
    "搞",
    "甚",
    "室",
    "待",
    "核",
    "校",
    "散",
    "侵",
    "吧",
    "甲",
    "遊",
    "久",
    "菜",
    "味",
    "舊",
    "模",
    "湖",
    "貨",
    "損",
    "預",
    "阻",
    "毫",
    "普",
    "穩",
    "乙",
    "媽",
    "植",
    "息",
    "擴",
    "銀",
    "語",
    "揮",
    "酒",
    "守",
    "拿",
    "序",
    "紙",
    "醫",
    "缺",
    "雨",
    "嗎",
    "針",
    "劉",
    "啊",
    "急",
    "唱",
    "誤",
    "訓",
    "願",
    "審",
    "附",
    "獲",
    "茶",
    "鮮",
    "糧",
    "斤",
    "孩",
    "脫",
    "硫",
    "肥",
    "善",
    "龍",
    "演",
    "父",
    "漸",
    "血",
    "歡",
    "械",
    "掌",
    "歌",
    "沙",
    "剛",
    "攻",
    "謂",
    "盾",
    "討",
    "晚",
    "粒",
    "亂",
    "燃",
    "矛",
    "乎",
    "殺",
    "藥",
    "寧",
    "魯",
    "貴",
    "鐘",
    "煤",
    "讀",
    "班",
    "伯",
    "香",
    "介",
    "迫",
    "句",
    "豐",
    "培",
    "握",
    "蘭",
    "擔",
    "弦",
    "蛋",
    "沉",
    "假",
    "穿",
    "執",
    "答",
    "樂",
    "誰",
    "順",
    "煙",
    "縮",
    "徵",
    "臉",
    "喜",
    "松",
    "腳",
    "困",
    "異",
    "免",
    "背",
    "星",
    "福",
    "買",
    "染",
    "井",
    "概",
    "慢",
    "怕",
    "磁",
    "倍",
    "祖",
    "皇",
    "促",
    "靜",
    "補",
    "評",
    "翻",
    "肉",
    "踐",
    "尼",
    "衣",
    "寬",
    "揚",
    "棉",
    "希",
    "傷",
    "操",
    "垂",
    "秋",
    "宜",
    "氫",
    "套",
    "督",
    "振",
    "架",
    "亮",
    "末",
    "憲",
    "慶",
    "編",
    "牛",
    "觸",
    "映",
    "雷",
    "銷",
    "詩",
    "座",
    "居",
    "抓",
    "裂",
    "胞",
    "呼",
    "娘",
    "景",
    "威",
    "綠",
    "晶",
    "厚",
    "盟",
    "衡",
    "雞",
    "孫",
    "延",
    "危",
    "膠",
    "屋",
    "鄉",
    "臨",
    "陸",
    "顧",
    "掉",
    "呀",
    "燈",
    "歲",
    "措",
    "束",
    "耐",
    "劇",
    "玉",
    "趙",
    "跳",
    "哥",
    "季",
    "課",
    "凱",
    "胡",
    "額",
    "款",
    "紹",
    "卷",
    "齊",
    "偉",
    "蒸",
    "殖",
    "永",
    "宗",
    "苗",
    "川",
    "爐",
    "岩",
    "弱",
    "零",
    "楊",
    "奏",
    "沿",
    "露",
    "桿",
    "探",
    "滑",
    "鎮",
    "飯",
    "濃",
    "航",
    "懷",
    "趕",
    "庫",
    "奪",
    "伊",
    "靈",
    "稅",
    "途",
    "滅",
    "賽",
    "歸",
    "召",
    "鼓",
    "播",
    "盤",
    "裁",
    "險",
    "康",
    "唯",
    "錄",
    "菌",
    "純",
    "借",
    "糖",
    "蓋",
    "橫",
    "符",
    "私",
    "努",
    "堂",
    "域",
    "槍",
    "潤",
    "幅",
    "哈",
    "竟",
    "熟",
    "蟲",
    "澤",
    "腦",
    "壤",
    "碳",
    "歐",
    "遍",
    "側",
    "寨",
    "敢",
    "徹",
    "慮",
    "斜",
    "薄",
    "庭",
    "納",
    "彈",
    "飼",
    "伸",
    "折",
    "麥",
    "濕",
    "暗",
    "荷",
    "瓦",
    "塞",
    "床",
    "築",
    "惡",
    "戶",
    "訪",
    "塔",
    "奇",
    "透",
    "梁",
    "刀",
    "旋",
    "跡",
    "卡",
    "氯",
    "遇",
    "份",
    "毒",
    "泥",
    "退",
    "洗",
    "擺",
    "灰",
    "彩",
    "賣",
    "耗",
    "夏",
    "擇",
    "忙",
    "銅",
    "獻",
    "硬",
    "予",
    "繁",
    "圈",
    "雪",
    "函",
    "亦",
    "抽",
    "篇",
    "陣",
    "陰",
    "丁",
    "尺",
    "追",
    "堆",
    "雄",
    "迎",
    "泛",
    "爸",
    "樓",
    "避",
    "謀",
    "噸",
    "野",
    "豬",
    "旗",
    "累",
    "偏",
    "典",
    "館",
    "索",
    "秦",
    "脂",
    "潮",
    "爺",
    "豆",
    "忽",
    "托",
    "驚",
    "塑",
    "遺",
    "愈",
    "朱",
    "替",
    "纖",
    "粗",
    "傾",
    "尚",
    "痛",
    "楚",
    "謝",
    "奮",
    "購",
    "磨",
    "君",
    "池",
    "旁",
    "碎",
    "骨",
    "監",
    "捕",
    "弟",
    "暴",
    "割",
    "貫",
    "殊",
    "釋",
    "詞",
    "亡",
    "壁",
    "頓",
    "寶",
    "午",
    "塵",
    "聞",
    "揭",
    "炮",
    "殘",
    "冬",
    "橋",
    "婦",
    "警",
    "綜",
    "招",
    "吳",
    "付",
    "浮",
    "遭",
    "徐",
    "您",
    "搖",
    "谷",
    "贊",
    "箱",
    "隔",
    "訂",
    "男",
    "吹",
    "園",
    "紛",
    "唐",
    "敗",
    "宋",
    "玻",
    "巨",
    "耕",
    "坦",
    "榮",
    "閉",
    "灣",
    "鍵",
    "凡",
    "駐",
    "鍋",
    "救",
    "恩",
    "剝",
    "凝",
    "鹼",
    "齒",
    "截",
    "煉",
    "麻",
    "紡",
    "禁",
    "廢",
    "盛",
    "版",
    "緩",
    "淨",
    "睛",
    "昌",
    "婚",
    "涉",
    "筒",
    "嘴",
    "插",
    "岸",
    "朗",
    "莊",
    "街",
    "藏",
    "姑",
    "貿",
    "腐",
    "奴",
    "啦",
    "慣",
    "乘",
    "夥",
    "恢",
    "勻",
    "紗",
    "扎",
    "辯",
    "耳",
    "彪",
    "臣",
    "億",
    "璃",
    "抵",
    "脈",
    "秀",
    "薩",
    "俄",
    "網",
    "舞",
    "店",
    "噴",
    "縱",
    "寸",
    "汗",
    "掛",
    "洪",
    "賀",
    "閃",
    "柬",
    "爆",
    "烯",
    "津",
    "稻",
    "牆",
    "軟",
    "勇",
    "像",
    "滾",
    "厘",
    "蒙",
    "芳",
    "肯",
    "坡",
    "柱",
    "盪",
    "腿",
    "儀",
    "旅",
    "尾",
    "軋",
    "冰",
    "貢",
    "登",
    "黎",
    "削",
    "鑽",
    "勒",
    "逃",
    "障",
    "氨",
    "郭",
    "峰",
    "幣",
    "港",
    "伏",
    "軌",
    "畝",
    "畢",
    "擦",
    "莫",
    "刺",
    "浪",
    "秘",
    "援",
    "株",
    "健",
    "售",
    "股",
    "島",
    "甘",
    "泡",
    "睡",
    "童",
    "鑄",
    "湯",
    "閥",
    "休",
    "匯",
    "舍",
    "牧",
    "繞",
    "炸",
    "哲",
    "磷",
    "績",
    "朋",
    "淡",
    "尖",
    "啟",
    "陷",
    "柴",
    "呈",
    "徒",
    "顏",
    "淚",
    "稍",
    "忘",
    "泵",
    "藍",
    "拖",
    "洞",
    "授",
    "鏡",
    "辛",
    "壯",
    "鋒",
    "貧",
    "虛",
    "彎",
    "摩",
    "泰",
    "幼",
    "廷",
    "尊",
    "窗",
    "綱",
    "弄",
    "隸",
    "疑",
    "氏",
    "宮",
    "姐",
    "震",
    "瑞",
    "怪",
    "尤",
    "琴",
    "循",
    "描",
    "膜",
    "違",
    "夾",
    "腰",
    "緣",
    "珠",
    "窮",
    "森",
    "枝",
    "竹",
    "溝",
    "催",
    "繩",
    "憶",
    "邦",
    "剩",
    "幸",
    "漿",
    "欄",
    "擁",
    "牙",
    "貯",
    "禮",
    "濾",
    "鈉",
    "紋",
    "罷",
    "拍",
    "咱",
    "喊",
    "袖",
    "埃",
    "勤",
    "罰",
    "焦",
    "潛",
    "伍",
    "墨",
    "欲",
    "縫",
    "姓",
    "刊",
    "飽",
    "仿",
    "獎",
    "鋁",
    "鬼",
    "麗",
    "跨",
    "默",
    "挖",
    "鏈",
    "掃",
    "喝",
    "袋",
    "炭",
    "污",
    "幕",
    "諸",
    "弧",
    "勵",
    "梅",
    "奶",
    "潔",
    "災",
    "舟",
    "鑑",
    "苯",
    "訟",
    "抱",
    "毀",
    "懂",
    "寒",
    "智",
    "埔",
    "寄",
    "屆",
    "躍",
    "渡",
    "挑",
    "丹",
    "艱",
    "貝",
    "碰",
    "拔",
    "爹",
    "戴",
    "碼",
    "夢",
    "芽",
    "熔",
    "赤",
    "漁",
    "哭",
    "敬",
    "顆",
    "奔",
    "鉛",
    "仲",
    "虎",
    "稀",
    "妹",
    "乏",
    "珍",
    "申",
    "桌",
    "遵",
    "允",
    "隆",
    "螺",
    "倉",
    "魏",
    "銳",
    "曉",
    "氮",
    "兼",
    "隱",
    "礙",
    "赫",
    "撥",
    "忠",
    "肅",
    "缸",
    "牽",
    "搶",
    "博",
    "巧",
    "殼",
    "兄",
    "杜",
    "訊",
    "誠",
    "碧",
    "祥",
    "柯",
    "頁",
    "巡",
    "矩",
    "悲",
    "灌",
    "齡",
    "倫",
    "票",
    "尋",
    "桂",
    "鋪",
    "聖",
    "恐",
    "恰",
    "鄭",
    "趣",
    "抬",
    "荒",
    "騰",
    "貼",
    "柔",
    "滴",
    "猛",
    "闊",
    "輛",
    "妻",
    "填",
    "撤",
    "儲",
    "簽",
    "鬧",
    "擾",
    "紫",
    "砂",
    "遞",
    "戲",
    "吊",
    "陶",
    "伐",
    "餵",
    "療",
    "瓶",
    "婆",
    "撫",
    "臂",
    "摸",
    "忍",
    "蝦",
    "蠟",
    "鄰",
    "胸",
    "鞏",
    "擠",
    "偶",
    "棄",
    "槽",
    "勁",
    "乳",
    "鄧",
    "吉",
    "仁",
    "爛",
    "磚",
    "租",
    "烏",
    "艦",
    "伴",
    "瓜",
    "淺",
    "丙",
    "暫",
    "燥",
    "橡",
    "柳",
    "迷",
    "暖",
    "牌",
    "秧",
    "膽",
    "詳",
    "簧",
    "踏",
    "瓷",
    "譜",
    "呆",
    "賓",
    "糊",
    "洛",
    "輝",
    "憤",
    "競",
    "隙",
    "怒",
    "粘",
    "乃",
    "緒",
    "肩",
    "籍",
    "敏",
    "塗",
    "熙",
    "皆",
    "偵",
    "懸",
    "掘",
    "享",
    "糾",
    "醒",
    "狂",
    "鎖",
    "淀",
    "恨",
    "牲",
    "霸",
    "爬",
    "賞",
    "逆",
    "玩",
    "陵",
    "祝",
    "秒",
    "浙",
    "貌",
    "役",
    "彼",
    "悉",
    "鴨",
    "趨",
    "鳳",
    "晨",
    "畜",
    "輩",
    "秩",
    "卵",
    "署",
    "梯",
    "炎",
    "灘",
    "棋",
    "驅",
    "篩",
    "峽",
    "冒",
    "啥",
    "壽",
    "譯",
    "浸",
    "泉",
    "帽",
    "遲",
    "矽",
    "疆",
    "貸",
    "漏",
    "稿",
    "冠",
    "嫩",
    "脅",
    "芯",
    "牢",
    "叛",
    "蝕",
    "奧",
    "鳴",
    "嶺",
    "羊",
    "憑",
    "串",
    "塘",
    "繪",
    "酵",
    "融",
    "盆",
    "錫",
    "廟",
    "籌",
    "凍",
    "輔",
    "攝",
    "襲",
    "筋",
    "拒",
    "僚",
    "旱",
    "鉀",
    "鳥",
    "漆",
    "沈",
    "眉",
    "疏",
    "添",
    "棒",
    "穗",
    "硝",
    "韓",
    "逼",
    "扭",
    "僑",
    "涼",
    "挺",
    "碗",
    "栽",
    "炒",
    "杯",
    "患",
    "餾",
    "勸",
    "豪",
    "遼",
    "勃",
    "鴻",
    "旦",
    "吏",
    "拜",
    "狗",
    "埋",
    "輥",
    "掩",
    "飲",
    "搬",
    "罵",
    "辭",
    "勾",
    "扣",
    "估",
    "蔣",
    "絨",
    "霧",
    "丈",
    "朵",
    "姆",
    "擬",
    "宇",
    "輯",
    "陝",
    "雕",
    "償",
    "蓄",
    "崇",
    "剪",
    "倡",
    "廳",
    "咬",
    "駛",
    "薯",
    "刷",
    "斥",
    "番",
    "賦",
    "奉",
    "佛",
    "澆",
    "漫",
    "曼",
    "扇",
    "鈣",
    "桃",
    "扶",
    "仔",
    "返",
    "俗",
    "虧",
    "腔",
    "鞋",
    "棱",
    "覆",
    "框",
    "悄",
    "叔",
    "撞",
    "騙",
    "勘",
    "旺",
    "沸",
    "孤",
    "吐",
    "孟",
    "渠",
    "屈",
    "疾",
    "妙",
    "惜",
    "仰",
    "狠",
    "脹",
    "諧",
    "拋",
    "黴",
    "桑",
    "崗",
    "嘛",
    "衰",
    "盜",
    "滲",
    "臟",
    "賴",
    "湧",
    "甜",
    "曹",
    "閱",
    "肌",
    "哩",
    "厲",
    "烴",
    "緯",
    "毅",
    "昨",
    "偽",
    "症",
    "煮",
    "嘆",
    "釘",
    "搭",
    "莖",
    "籠",
    "酷",
    "偷",
    "弓",
    "錐",
    "恆",
    "傑",
    "坑",
    "鼻",
    "翼",
    "綸",
    "敘",
    "獄",
    "逮",
    "罐",
    "絡",
    "棚",
    "抑",
    "膨",
    "蔬",
    "寺",
    "驟",
    "穆",
    "冶",
    "枯",
    "冊",
    "屍",
    "凸",
    "紳",
    "坯",
    "犧",
    "焰",
    "轟",
    "欣",
    "晉",
    "瘦",
    "禦",
    "錠",
    "錦",
    "喪",
    "旬",
    "鍛",
    "壟",
    "搜",
    "撲",
    "邀",
    "亭",
    "酯",
    "邁",
    "舒",
    "脆",
    "酶",
    "閒",
    "憂",
    "酚",
    "頑",
    "羽",
    "漲",
    "卸",
    "仗",
    "陪",
    "闢",
    "懲",
    "杭",
    "姚",
    "肚",
    "捉",
    "飄",
    "漂",
    "昆",
    "欺",
    "吾",
    "郎",
    "烷",
    "汁",
    "呵",
    "飾",
    "蕭",
    "雅",
    "郵",
    "遷",
    "燕",
    "撒",
    "姻",
    "赴",
    "宴",
    "煩",
    "債",
    "帳",
    "斑",
    "鈴",
    "旨",
    "醇",
    "董",
    "餅",
    "雛",
    "姿",
    "拌",
    "傅",
    "腹",
    "妥",
    "揉",
    "賢",
    "拆",
    "歪",
    "葡",
    "胺",
    "丟",
    "浩",
    "徽",
    "昂",
    "墊",
    "擋",
    "覽",
    "貪",
    "慰",
    "繳",
    "汪",
    "慌",
    "馮",
    "諾",
    "姜",
    "誼",
    "兇",
    "劣",
    "誣",
    "耀",
    "昏",
    "躺",
    "盈",
    "騎",
    "喬",
    "溪",
    "叢",
    "盧",
    "抹",
    "悶",
    "諮",
    "刮",
    "駕",
    "纜",
    "悟",
    "摘",
    "鉺",
    "擲",
    "頗",
    "幻",
    "柄",
    "惠",
    "慘",
    "佳",
    "仇",
    "臘",
    "窩",
    "滌",
    "劍",
    "瞧",
    "堡",
    "潑",
    "蔥",
    "罩",
    "霍",
    "撈",
    "胎",
    "蒼",
    "濱",
    "倆",
    "捅",
    "湘",
    "砍",
    "霞",
    "邵",
    "萄",
    "瘋",
    "淮",
    "遂",
    "熊",
    "糞",
    "烘",
    "宿",
    "檔",
    "戈",
    "駁",
    "嫂",
    "裕",
    "徙",
    "箭",
    "捐",
    "腸",
    "撐",
    "曬",
    "辨",
    "殿",
    "蓮",
    "攤",
    "攪",
    "醬",
    "屏",
    "疫",
    "哀",
    "蔡",
    "堵",
    "沫",
    "皺",
    "暢",
    "疊",
    "閣",
    "萊",
    "敲",
    "轄",
    "鉤",
    "痕",
    "壩",
    "巷",
    "餓",
    "禍",
    "丘",
    "玄",
    "溜",
    "曰",
    "邏",
    "彭",
    "嘗",
    "卿",
    "妨",
    "艇",
    "吞",
    "韋",
    "怨",
    "矮",
    "歇"
]

},{}],11:[function(require,module,exports){
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

},{}],12:[function(require,module,exports){
module.exports=[
    "abaisser",
    "abandon",
    "abdiquer",
    "abeille",
    "abolir",
    "aborder",
    "aboutir",
    "aboyer",
    "abrasif",
    "abreuver",
    "abriter",
    "abroger",
    "abrupt",
    "absence",
    "absolu",
    "absurde",
    "abusif",
    "abyssal",
    "académie",
    "acajou",
    "acarien",
    "accabler",
    "accepter",
    "acclamer",
    "accolade",
    "accroche",
    "accuser",
    "acerbe",
    "achat",
    "acheter",
    "aciduler",
    "acier",
    "acompte",
    "acquérir",
    "acronyme",
    "acteur",
    "actif",
    "actuel",
    "adepte",
    "adéquat",
    "adhésif",
    "adjectif",
    "adjuger",
    "admettre",
    "admirer",
    "adopter",
    "adorer",
    "adoucir",
    "adresse",
    "adroit",
    "adulte",
    "adverbe",
    "aérer",
    "aéronef",
    "affaire",
    "affecter",
    "affiche",
    "affreux",
    "affubler",
    "agacer",
    "agencer",
    "agile",
    "agiter",
    "agrafer",
    "agréable",
    "agrume",
    "aider",
    "aiguille",
    "ailier",
    "aimable",
    "aisance",
    "ajouter",
    "ajuster",
    "alarmer",
    "alchimie",
    "alerte",
    "algèbre",
    "algue",
    "aliéner",
    "aliment",
    "alléger",
    "alliage",
    "allouer",
    "allumer",
    "alourdir",
    "alpaga",
    "altesse",
    "alvéole",
    "amateur",
    "ambigu",
    "ambre",
    "aménager",
    "amertume",
    "amidon",
    "amiral",
    "amorcer",
    "amour",
    "amovible",
    "amphibie",
    "ampleur",
    "amusant",
    "analyse",
    "anaphore",
    "anarchie",
    "anatomie",
    "ancien",
    "anéantir",
    "angle",
    "angoisse",
    "anguleux",
    "animal",
    "annexer",
    "annonce",
    "annuel",
    "anodin",
    "anomalie",
    "anonyme",
    "anormal",
    "antenne",
    "antidote",
    "anxieux",
    "apaiser",
    "apéritif",
    "aplanir",
    "apologie",
    "appareil",
    "appeler",
    "apporter",
    "appuyer",
    "aquarium",
    "aqueduc",
    "arbitre",
    "arbuste",
    "ardeur",
    "ardoise",
    "argent",
    "arlequin",
    "armature",
    "armement",
    "armoire",
    "armure",
    "arpenter",
    "arracher",
    "arriver",
    "arroser",
    "arsenic",
    "artériel",
    "article",
    "aspect",
    "asphalte",
    "aspirer",
    "assaut",
    "asservir",
    "assiette",
    "associer",
    "assurer",
    "asticot",
    "astre",
    "astuce",
    "atelier",
    "atome",
    "atrium",
    "atroce",
    "attaque",
    "attentif",
    "attirer",
    "attraper",
    "aubaine",
    "auberge",
    "audace",
    "audible",
    "augurer",
    "aurore",
    "automne",
    "autruche",
    "avaler",
    "avancer",
    "avarice",
    "avenir",
    "averse",
    "aveugle",
    "aviateur",
    "avide",
    "avion",
    "aviser",
    "avoine",
    "avouer",
    "avril",
    "axial",
    "axiome",
    "badge",
    "bafouer",
    "bagage",
    "baguette",
    "baignade",
    "balancer",
    "balcon",
    "baleine",
    "balisage",
    "bambin",
    "bancaire",
    "bandage",
    "banlieue",
    "bannière",
    "banquier",
    "barbier",
    "baril",
    "baron",
    "barque",
    "barrage",
    "bassin",
    "bastion",
    "bataille",
    "bateau",
    "batterie",
    "baudrier",
    "bavarder",
    "belette",
    "bélier",
    "belote",
    "bénéfice",
    "berceau",
    "berger",
    "berline",
    "bermuda",
    "besace",
    "besogne",
    "bétail",
    "beurre",
    "biberon",
    "bicycle",
    "bidule",
    "bijou",
    "bilan",
    "bilingue",
    "billard",
    "binaire",
    "biologie",
    "biopsie",
    "biotype",
    "biscuit",
    "bison",
    "bistouri",
    "bitume",
    "bizarre",
    "blafard",
    "blague",
    "blanchir",
    "blessant",
    "blinder",
    "blond",
    "bloquer",
    "blouson",
    "bobard",
    "bobine",
    "boire",
    "boiser",
    "bolide",
    "bonbon",
    "bondir",
    "bonheur",
    "bonifier",
    "bonus",
    "bordure",
    "borne",
    "botte",
    "boucle",
    "boueux",
    "bougie",
    "boulon",
    "bouquin",
    "bourse",
    "boussole",
    "boutique",
    "boxeur",
    "branche",
    "brasier",
    "brave",
    "brebis",
    "brèche",
    "breuvage",
    "bricoler",
    "brigade",
    "brillant",
    "brioche",
    "brique",
    "brochure",
    "broder",
    "bronzer",
    "brousse",
    "broyeur",
    "brume",
    "brusque",
    "brutal",
    "bruyant",
    "buffle",
    "buisson",
    "bulletin",
    "bureau",
    "burin",
    "bustier",
    "butiner",
    "butoir",
    "buvable",
    "buvette",
    "cabanon",
    "cabine",
    "cachette",
    "cadeau",
    "cadre",
    "caféine",
    "caillou",
    "caisson",
    "calculer",
    "calepin",
    "calibre",
    "calmer",
    "calomnie",
    "calvaire",
    "camarade",
    "caméra",
    "camion",
    "campagne",
    "canal",
    "caneton",
    "canon",
    "cantine",
    "canular",
    "capable",
    "caporal",
    "caprice",
    "capsule",
    "capter",
    "capuche",
    "carabine",
    "carbone",
    "caresser",
    "caribou",
    "carnage",
    "carotte",
    "carreau",
    "carton",
    "cascade",
    "casier",
    "casque",
    "cassure",
    "causer",
    "caution",
    "cavalier",
    "caverne",
    "caviar",
    "cédille",
    "ceinture",
    "céleste",
    "cellule",
    "cendrier",
    "censurer",
    "central",
    "cercle",
    "cérébral",
    "cerise",
    "cerner",
    "cerveau",
    "cesser",
    "chagrin",
    "chaise",
    "chaleur",
    "chambre",
    "chance",
    "chapitre",
    "charbon",
    "chasseur",
    "chaton",
    "chausson",
    "chavirer",
    "chemise",
    "chenille",
    "chéquier",
    "chercher",
    "cheval",
    "chien",
    "chiffre",
    "chignon",
    "chimère",
    "chiot",
    "chlorure",
    "chocolat",
    "choisir",
    "chose",
    "chouette",
    "chrome",
    "chute",
    "cigare",
    "cigogne",
    "cimenter",
    "cinéma",
    "cintrer",
    "circuler",
    "cirer",
    "cirque",
    "citerne",
    "citoyen",
    "citron",
    "civil",
    "clairon",
    "clameur",
    "claquer",
    "classe",
    "clavier",
    "client",
    "cligner",
    "climat",
    "clivage",
    "cloche",
    "clonage",
    "cloporte",
    "cobalt",
    "cobra",
    "cocasse",
    "cocotier",
    "coder",
    "codifier",
    "coffre",
    "cogner",
    "cohésion",
    "coiffer",
    "coincer",
    "colère",
    "colibri",
    "colline",
    "colmater",
    "colonel",
    "combat",
    "comédie",
    "commande",
    "compact",
    "concert",
    "conduire",
    "confier",
    "congeler",
    "connoter",
    "consonne",
    "contact",
    "convexe",
    "copain",
    "copie",
    "corail",
    "corbeau",
    "cordage",
    "corniche",
    "corpus",
    "correct",
    "cortège",
    "cosmique",
    "costume",
    "coton",
    "coude",
    "coupure",
    "courage",
    "couteau",
    "couvrir",
    "coyote",
    "crabe",
    "crainte",
    "cravate",
    "crayon",
    "créature",
    "créditer",
    "crémeux",
    "creuser",
    "crevette",
    "cribler",
    "crier",
    "cristal",
    "critère",
    "croire",
    "croquer",
    "crotale",
    "crucial",
    "cruel",
    "crypter",
    "cubique",
    "cueillir",
    "cuillère",
    "cuisine",
    "cuivre",
    "culminer",
    "cultiver",
    "cumuler",
    "cupide",
    "curatif",
    "curseur",
    "cyanure",
    "cycle",
    "cylindre",
    "cynique",
    "daigner",
    "damier",
    "danger",
    "danseur",
    "dauphin",
    "débattre",
    "débiter",
    "déborder",
    "débrider",
    "débutant",
    "décaler",
    "décembre",
    "déchirer",
    "décider",
    "déclarer",
    "décorer",
    "décrire",
    "décupler",
    "dédale",
    "déductif",
    "déesse",
    "défensif",
    "défiler",
    "défrayer",
    "dégager",
    "dégivrer",
    "déglutir",
    "dégrafer",
    "déjeuner",
    "délice",
    "déloger",
    "demander",
    "demeurer",
    "démolir",
    "dénicher",
    "dénouer",
    "dentelle",
    "dénuder",
    "départ",
    "dépenser",
    "déphaser",
    "déplacer",
    "déposer",
    "déranger",
    "dérober",
    "désastre",
    "descente",
    "désert",
    "désigner",
    "désobéir",
    "dessiner",
    "destrier",
    "détacher",
    "détester",
    "détourer",
    "détresse",
    "devancer",
    "devenir",
    "deviner",
    "devoir",
    "diable",
    "dialogue",
    "diamant",
    "dicter",
    "différer",
    "digérer",
    "digital",
    "digne",
    "diluer",
    "dimanche",
    "diminuer",
    "dioxyde",
    "directif",
    "diriger",
    "discuter",
    "disposer",
    "dissiper",
    "distance",
    "divertir",
    "diviser",
    "docile",
    "docteur",
    "dogme",
    "doigt",
    "domaine",
    "domicile",
    "dompter",
    "donateur",
    "donjon",
    "donner",
    "dopamine",
    "dortoir",
    "dorure",
    "dosage",
    "doseur",
    "dossier",
    "dotation",
    "douanier",
    "double",
    "douceur",
    "douter",
    "doyen",
    "dragon",
    "draper",
    "dresser",
    "dribbler",
    "droiture",
    "duperie",
    "duplexe",
    "durable",
    "durcir",
    "dynastie",
    "éblouir",
    "écarter",
    "écharpe",
    "échelle",
    "éclairer",
    "éclipse",
    "éclore",
    "écluse",
    "école",
    "économie",
    "écorce",
    "écouter",
    "écraser",
    "écrémer",
    "écrivain",
    "écrou",
    "écume",
    "écureuil",
    "édifier",
    "éduquer",
    "effacer",
    "effectif",
    "effigie",
    "effort",
    "effrayer",
    "effusion",
    "égaliser",
    "égarer",
    "éjecter",
    "élaborer",
    "élargir",
    "électron",
    "élégant",
    "éléphant",
    "élève",
    "éligible",
    "élitisme",
    "éloge",
    "élucider",
    "éluder",
    "emballer",
    "embellir",
    "embryon",
    "émeraude",
    "émission",
    "emmener",
    "émotion",
    "émouvoir",
    "empereur",
    "employer",
    "emporter",
    "emprise",
    "émulsion",
    "encadrer",
    "enchère",
    "enclave",
    "encoche",
    "endiguer",
    "endosser",
    "endroit",
    "enduire",
    "énergie",
    "enfance",
    "enfermer",
    "enfouir",
    "engager",
    "engin",
    "englober",
    "énigme",
    "enjamber",
    "enjeu",
    "enlever",
    "ennemi",
    "ennuyeux",
    "enrichir",
    "enrobage",
    "enseigne",
    "entasser",
    "entendre",
    "entier",
    "entourer",
    "entraver",
    "énumérer",
    "envahir",
    "enviable",
    "envoyer",
    "enzyme",
    "éolien",
    "épaissir",
    "épargne",
    "épatant",
    "épaule",
    "épicerie",
    "épidémie",
    "épier",
    "épilogue",
    "épine",
    "épisode",
    "épitaphe",
    "époque",
    "épreuve",
    "éprouver",
    "épuisant",
    "équerre",
    "équipe",
    "ériger",
    "érosion",
    "erreur",
    "éruption",
    "escalier",
    "espadon",
    "espèce",
    "espiègle",
    "espoir",
    "esprit",
    "esquiver",
    "essayer",
    "essence",
    "essieu",
    "essorer",
    "estime",
    "estomac",
    "estrade",
    "étagère",
    "étaler",
    "étanche",
    "étatique",
    "éteindre",
    "étendoir",
    "éternel",
    "éthanol",
    "éthique",
    "ethnie",
    "étirer",
    "étoffer",
    "étoile",
    "étonnant",
    "étourdir",
    "étrange",
    "étroit",
    "étude",
    "euphorie",
    "évaluer",
    "évasion",
    "éventail",
    "évidence",
    "éviter",
    "évolutif",
    "évoquer",
    "exact",
    "exagérer",
    "exaucer",
    "exceller",
    "excitant",
    "exclusif",
    "excuse",
    "exécuter",
    "exemple",
    "exercer",
    "exhaler",
    "exhorter",
    "exigence",
    "exiler",
    "exister",
    "exotique",
    "expédier",
    "explorer",
    "exposer",
    "exprimer",
    "exquis",
    "extensif",
    "extraire",
    "exulter",
    "fable",
    "fabuleux",
    "facette",
    "facile",
    "facture",
    "faiblir",
    "falaise",
    "fameux",
    "famille",
    "farceur",
    "farfelu",
    "farine",
    "farouche",
    "fasciner",
    "fatal",
    "fatigue",
    "faucon",
    "fautif",
    "faveur",
    "favori",
    "fébrile",
    "féconder",
    "fédérer",
    "félin",
    "femme",
    "fémur",
    "fendoir",
    "féodal",
    "fermer",
    "féroce",
    "ferveur",
    "festival",
    "feuille",
    "feutre",
    "février",
    "fiasco",
    "ficeler",
    "fictif",
    "fidèle",
    "figure",
    "filature",
    "filetage",
    "filière",
    "filleul",
    "filmer",
    "filou",
    "filtrer",
    "financer",
    "finir",
    "fiole",
    "firme",
    "fissure",
    "fixer",
    "flairer",
    "flamme",
    "flasque",
    "flatteur",
    "fléau",
    "flèche",
    "fleur",
    "flexion",
    "flocon",
    "flore",
    "fluctuer",
    "fluide",
    "fluvial",
    "folie",
    "fonderie",
    "fongible",
    "fontaine",
    "forcer",
    "forgeron",
    "formuler",
    "fortune",
    "fossile",
    "foudre",
    "fougère",
    "fouiller",
    "foulure",
    "fourmi",
    "fragile",
    "fraise",
    "franchir",
    "frapper",
    "frayeur",
    "frégate",
    "freiner",
    "frelon",
    "frémir",
    "frénésie",
    "frère",
    "friable",
    "friction",
    "frisson",
    "frivole",
    "froid",
    "fromage",
    "frontal",
    "frotter",
    "fruit",
    "fugitif",
    "fuite",
    "fureur",
    "furieux",
    "furtif",
    "fusion",
    "futur",
    "gagner",
    "galaxie",
    "galerie",
    "gambader",
    "garantir",
    "gardien",
    "garnir",
    "garrigue",
    "gazelle",
    "gazon",
    "géant",
    "gélatine",
    "gélule",
    "gendarme",
    "général",
    "génie",
    "genou",
    "gentil",
    "géologie",
    "géomètre",
    "géranium",
    "germe",
    "gestuel",
    "geyser",
    "gibier",
    "gicler",
    "girafe",
    "givre",
    "glace",
    "glaive",
    "glisser",
    "globe",
    "gloire",
    "glorieux",
    "golfeur",
    "gomme",
    "gonfler",
    "gorge",
    "gorille",
    "goudron",
    "gouffre",
    "goulot",
    "goupille",
    "gourmand",
    "goutte",
    "graduel",
    "graffiti",
    "graine",
    "grand",
    "grappin",
    "gratuit",
    "gravir",
    "grenat",
    "griffure",
    "griller",
    "grimper",
    "grogner",
    "gronder",
    "grotte",
    "groupe",
    "gruger",
    "grutier",
    "gruyère",
    "guépard",
    "guerrier",
    "guide",
    "guimauve",
    "guitare",
    "gustatif",
    "gymnaste",
    "gyrostat",
    "habitude",
    "hachoir",
    "halte",
    "hameau",
    "hangar",
    "hanneton",
    "haricot",
    "harmonie",
    "harpon",
    "hasard",
    "hélium",
    "hématome",
    "herbe",
    "hérisson",
    "hermine",
    "héron",
    "hésiter",
    "heureux",
    "hiberner",
    "hibou",
    "hilarant",
    "histoire",
    "hiver",
    "homard",
    "hommage",
    "homogène",
    "honneur",
    "honorer",
    "honteux",
    "horde",
    "horizon",
    "horloge",
    "hormone",
    "horrible",
    "houleux",
    "housse",
    "hublot",
    "huileux",
    "humain",
    "humble",
    "humide",
    "humour",
    "hurler",
    "hydromel",
    "hygiène",
    "hymne",
    "hypnose",
    "idylle",
    "ignorer",
    "iguane",
    "illicite",
    "illusion",
    "image",
    "imbiber",
    "imiter",
    "immense",
    "immobile",
    "immuable",
    "impact",
    "impérial",
    "implorer",
    "imposer",
    "imprimer",
    "imputer",
    "incarner",
    "incendie",
    "incident",
    "incliner",
    "incolore",
    "indexer",
    "indice",
    "inductif",
    "inédit",
    "ineptie",
    "inexact",
    "infini",
    "infliger",
    "informer",
    "infusion",
    "ingérer",
    "inhaler",
    "inhiber",
    "injecter",
    "injure",
    "innocent",
    "inoculer",
    "inonder",
    "inscrire",
    "insecte",
    "insigne",
    "insolite",
    "inspirer",
    "instinct",
    "insulter",
    "intact",
    "intense",
    "intime",
    "intrigue",
    "intuitif",
    "inutile",
    "invasion",
    "inventer",
    "inviter",
    "invoquer",
    "ironique",
    "irradier",
    "irréel",
    "irriter",
    "isoler",
    "ivoire",
    "ivresse",
    "jaguar",
    "jaillir",
    "jambe",
    "janvier",
    "jardin",
    "jauger",
    "jaune",
    "javelot",
    "jetable",
    "jeton",
    "jeudi",
    "jeunesse",
    "joindre",
    "joncher",
    "jongler",
    "joueur",
    "jouissif",
    "journal",
    "jovial",
    "joyau",
    "joyeux",
    "jubiler",
    "jugement",
    "junior",
    "jupon",
    "juriste",
    "justice",
    "juteux",
    "juvénile",
    "kayak",
    "kimono",
    "kiosque",
    "label",
    "labial",
    "labourer",
    "lacérer",
    "lactose",
    "lagune",
    "laine",
    "laisser",
    "laitier",
    "lambeau",
    "lamelle",
    "lampe",
    "lanceur",
    "langage",
    "lanterne",
    "lapin",
    "largeur",
    "larme",
    "laurier",
    "lavabo",
    "lavoir",
    "lecture",
    "légal",
    "léger",
    "légume",
    "lessive",
    "lettre",
    "levier",
    "lexique",
    "lézard",
    "liasse",
    "libérer",
    "libre",
    "licence",
    "licorne",
    "liège",
    "lièvre",
    "ligature",
    "ligoter",
    "ligue",
    "limer",
    "limite",
    "limonade",
    "limpide",
    "linéaire",
    "lingot",
    "lionceau",
    "liquide",
    "lisière",
    "lister",
    "lithium",
    "litige",
    "littoral",
    "livreur",
    "logique",
    "lointain",
    "loisir",
    "lombric",
    "loterie",
    "louer",
    "lourd",
    "loutre",
    "louve",
    "loyal",
    "lubie",
    "lucide",
    "lucratif",
    "lueur",
    "lugubre",
    "luisant",
    "lumière",
    "lunaire",
    "lundi",
    "luron",
    "lutter",
    "luxueux",
    "machine",
    "magasin",
    "magenta",
    "magique",
    "maigre",
    "maillon",
    "maintien",
    "mairie",
    "maison",
    "majorer",
    "malaxer",
    "maléfice",
    "malheur",
    "malice",
    "mallette",
    "mammouth",
    "mandater",
    "maniable",
    "manquant",
    "manteau",
    "manuel",
    "marathon",
    "marbre",
    "marchand",
    "mardi",
    "maritime",
    "marqueur",
    "marron",
    "marteler",
    "mascotte",
    "massif",
    "matériel",
    "matière",
    "matraque",
    "maudire",
    "maussade",
    "mauve",
    "maximal",
    "méchant",
    "méconnu",
    "médaille",
    "médecin",
    "méditer",
    "méduse",
    "meilleur",
    "mélange",
    "mélodie",
    "membre",
    "mémoire",
    "menacer",
    "mener",
    "menhir",
    "mensonge",
    "mentor",
    "mercredi",
    "mérite",
    "merle",
    "messager",
    "mesure",
    "métal",
    "météore",
    "méthode",
    "métier",
    "meuble",
    "miauler",
    "microbe",
    "miette",
    "mignon",
    "migrer",
    "milieu",
    "million",
    "mimique",
    "mince",
    "minéral",
    "minimal",
    "minorer",
    "minute",
    "miracle",
    "miroiter",
    "missile",
    "mixte",
    "mobile",
    "moderne",
    "moelleux",
    "mondial",
    "moniteur",
    "monnaie",
    "monotone",
    "monstre",
    "montagne",
    "monument",
    "moqueur",
    "morceau",
    "morsure",
    "mortier",
    "moteur",
    "motif",
    "mouche",
    "moufle",
    "moulin",
    "mousson",
    "mouton",
    "mouvant",
    "multiple",
    "munition",
    "muraille",
    "murène",
    "murmure",
    "muscle",
    "muséum",
    "musicien",
    "mutation",
    "muter",
    "mutuel",
    "myriade",
    "myrtille",
    "mystère",
    "mythique",
    "nageur",
    "nappe",
    "narquois",
    "narrer",
    "natation",
    "nation",
    "nature",
    "naufrage",
    "nautique",
    "navire",
    "nébuleux",
    "nectar",
    "néfaste",
    "négation",
    "négliger",
    "négocier",
    "neige",
    "nerveux",
    "nettoyer",
    "neurone",
    "neutron",
    "neveu",
    "niche",
    "nickel",
    "nitrate",
    "niveau",
    "noble",
    "nocif",
    "nocturne",
    "noirceur",
    "noisette",
    "nomade",
    "nombreux",
    "nommer",
    "normatif",
    "notable",
    "notifier",
    "notoire",
    "nourrir",
    "nouveau",
    "novateur",
    "novembre",
    "novice",
    "nuage",
    "nuancer",
    "nuire",
    "nuisible",
    "numéro",
    "nuptial",
    "nuque",
    "nutritif",
    "obéir",
    "objectif",
    "obliger",
    "obscur",
    "observer",
    "obstacle",
    "obtenir",
    "obturer",
    "occasion",
    "occuper",
    "océan",
    "octobre",
    "octroyer",
    "octupler",
    "oculaire",
    "odeur",
    "odorant",
    "offenser",
    "officier",
    "offrir",
    "ogive",
    "oiseau",
    "oisillon",
    "olfactif",
    "olivier",
    "ombrage",
    "omettre",
    "onctueux",
    "onduler",
    "onéreux",
    "onirique",
    "opale",
    "opaque",
    "opérer",
    "opinion",
    "opportun",
    "opprimer",
    "opter",
    "optique",
    "orageux",
    "orange",
    "orbite",
    "ordonner",
    "oreille",
    "organe",
    "orgueil",
    "orifice",
    "ornement",
    "orque",
    "ortie",
    "osciller",
    "osmose",
    "ossature",
    "otarie",
    "ouragan",
    "ourson",
    "outil",
    "outrager",
    "ouvrage",
    "ovation",
    "oxyde",
    "oxygène",
    "ozone",
    "paisible",
    "palace",
    "palmarès",
    "palourde",
    "palper",
    "panache",
    "panda",
    "pangolin",
    "paniquer",
    "panneau",
    "panorama",
    "pantalon",
    "papaye",
    "papier",
    "papoter",
    "papyrus",
    "paradoxe",
    "parcelle",
    "paresse",
    "parfumer",
    "parler",
    "parole",
    "parrain",
    "parsemer",
    "partager",
    "parure",
    "parvenir",
    "passion",
    "pastèque",
    "paternel",
    "patience",
    "patron",
    "pavillon",
    "pavoiser",
    "payer",
    "paysage",
    "peigne",
    "peintre",
    "pelage",
    "pélican",
    "pelle",
    "pelouse",
    "peluche",
    "pendule",
    "pénétrer",
    "pénible",
    "pensif",
    "pénurie",
    "pépite",
    "péplum",
    "perdrix",
    "perforer",
    "période",
    "permuter",
    "perplexe",
    "persil",
    "perte",
    "peser",
    "pétale",
    "petit",
    "pétrir",
    "peuple",
    "pharaon",
    "phobie",
    "phoque",
    "photon",
    "phrase",
    "physique",
    "piano",
    "pictural",
    "pièce",
    "pierre",
    "pieuvre",
    "pilote",
    "pinceau",
    "pipette",
    "piquer",
    "pirogue",
    "piscine",
    "piston",
    "pivoter",
    "pixel",
    "pizza",
    "placard",
    "plafond",
    "plaisir",
    "planer",
    "plaque",
    "plastron",
    "plateau",
    "pleurer",
    "plexus",
    "pliage",
    "plomb",
    "plonger",
    "pluie",
    "plumage",
    "pochette",
    "poésie",
    "poète",
    "pointe",
    "poirier",
    "poisson",
    "poivre",
    "polaire",
    "policier",
    "pollen",
    "polygone",
    "pommade",
    "pompier",
    "ponctuel",
    "pondérer",
    "poney",
    "portique",
    "position",
    "posséder",
    "posture",
    "potager",
    "poteau",
    "potion",
    "pouce",
    "poulain",
    "poumon",
    "pourpre",
    "poussin",
    "pouvoir",
    "prairie",
    "pratique",
    "précieux",
    "prédire",
    "préfixe",
    "prélude",
    "prénom",
    "présence",
    "prétexte",
    "prévoir",
    "primitif",
    "prince",
    "prison",
    "priver",
    "problème",
    "procéder",
    "prodige",
    "profond",
    "progrès",
    "proie",
    "projeter",
    "prologue",
    "promener",
    "propre",
    "prospère",
    "protéger",
    "prouesse",
    "proverbe",
    "prudence",
    "pruneau",
    "psychose",
    "public",
    "puceron",
    "puiser",
    "pulpe",
    "pulsar",
    "punaise",
    "punitif",
    "pupitre",
    "purifier",
    "puzzle",
    "pyramide",
    "quasar",
    "querelle",
    "question",
    "quiétude",
    "quitter",
    "quotient",
    "racine",
    "raconter",
    "radieux",
    "ragondin",
    "raideur",
    "raisin",
    "ralentir",
    "rallonge",
    "ramasser",
    "rapide",
    "rasage",
    "ratisser",
    "ravager",
    "ravin",
    "rayonner",
    "réactif",
    "réagir",
    "réaliser",
    "réanimer",
    "recevoir",
    "réciter",
    "réclamer",
    "récolter",
    "recruter",
    "reculer",
    "recycler",
    "rédiger",
    "redouter",
    "refaire",
    "réflexe",
    "réformer",
    "refrain",
    "refuge",
    "régalien",
    "région",
    "réglage",
    "régulier",
    "réitérer",
    "rejeter",
    "rejouer",
    "relatif",
    "relever",
    "relief",
    "remarque",
    "remède",
    "remise",
    "remonter",
    "remplir",
    "remuer",
    "renard",
    "renfort",
    "renifler",
    "renoncer",
    "rentrer",
    "renvoi",
    "replier",
    "reporter",
    "reprise",
    "reptile",
    "requin",
    "réserve",
    "résineux",
    "résoudre",
    "respect",
    "rester",
    "résultat",
    "rétablir",
    "retenir",
    "réticule",
    "retomber",
    "retracer",
    "réunion",
    "réussir",
    "revanche",
    "revivre",
    "révolte",
    "révulsif",
    "richesse",
    "rideau",
    "rieur",
    "rigide",
    "rigoler",
    "rincer",
    "riposter",
    "risible",
    "risque",
    "rituel",
    "rival",
    "rivière",
    "rocheux",
    "romance",
    "rompre",
    "ronce",
    "rondin",
    "roseau",
    "rosier",
    "rotatif",
    "rotor",
    "rotule",
    "rouge",
    "rouille",
    "rouleau",
    "routine",
    "royaume",
    "ruban",
    "rubis",
    "ruche",
    "ruelle",
    "rugueux",
    "ruiner",
    "ruisseau",
    "ruser",
    "rustique",
    "rythme",
    "sabler",
    "saboter",
    "sabre",
    "sacoche",
    "safari",
    "sagesse",
    "saisir",
    "salade",
    "salive",
    "salon",
    "saluer",
    "samedi",
    "sanction",
    "sanglier",
    "sarcasme",
    "sardine",
    "saturer",
    "saugrenu",
    "saumon",
    "sauter",
    "sauvage",
    "savant",
    "savonner",
    "scalpel",
    "scandale",
    "scélérat",
    "scénario",
    "sceptre",
    "schéma",
    "science",
    "scinder",
    "score",
    "scrutin",
    "sculpter",
    "séance",
    "sécable",
    "sécher",
    "secouer",
    "sécréter",
    "sédatif",
    "séduire",
    "seigneur",
    "séjour",
    "sélectif",
    "semaine",
    "sembler",
    "semence",
    "séminal",
    "sénateur",
    "sensible",
    "sentence",
    "séparer",
    "séquence",
    "serein",
    "sergent",
    "sérieux",
    "serrure",
    "sérum",
    "service",
    "sésame",
    "sévir",
    "sevrage",
    "sextuple",
    "sidéral",
    "siècle",
    "siéger",
    "siffler",
    "sigle",
    "signal",
    "silence",
    "silicium",
    "simple",
    "sincère",
    "sinistre",
    "siphon",
    "sirop",
    "sismique",
    "situer",
    "skier",
    "social",
    "socle",
    "sodium",
    "soigneux",
    "soldat",
    "soleil",
    "solitude",
    "soluble",
    "sombre",
    "sommeil",
    "somnoler",
    "sonde",
    "songeur",
    "sonnette",
    "sonore",
    "sorcier",
    "sortir",
    "sosie",
    "sottise",
    "soucieux",
    "soudure",
    "souffle",
    "soulever",
    "soupape",
    "source",
    "soutirer",
    "souvenir",
    "spacieux",
    "spatial",
    "spécial",
    "sphère",
    "spiral",
    "stable",
    "station",
    "sternum",
    "stimulus",
    "stipuler",
    "strict",
    "studieux",
    "stupeur",
    "styliste",
    "sublime",
    "substrat",
    "subtil",
    "subvenir",
    "succès",
    "sucre",
    "suffixe",
    "suggérer",
    "suiveur",
    "sulfate",
    "superbe",
    "supplier",
    "surface",
    "suricate",
    "surmener",
    "surprise",
    "sursaut",
    "survie",
    "suspect",
    "syllabe",
    "symbole",
    "symétrie",
    "synapse",
    "syntaxe",
    "système",
    "tabac",
    "tablier",
    "tactile",
    "tailler",
    "talent",
    "talisman",
    "talonner",
    "tambour",
    "tamiser",
    "tangible",
    "tapis",
    "taquiner",
    "tarder",
    "tarif",
    "tartine",
    "tasse",
    "tatami",
    "tatouage",
    "taupe",
    "taureau",
    "taxer",
    "témoin",
    "temporel",
    "tenaille",
    "tendre",
    "teneur",
    "tenir",
    "tension",
    "terminer",
    "terne",
    "terrible",
    "tétine",
    "texte",
    "thème",
    "théorie",
    "thérapie",
    "thorax",
    "tibia",
    "tiède",
    "timide",
    "tirelire",
    "tiroir",
    "tissu",
    "titane",
    "titre",
    "tituber",
    "toboggan",
    "tolérant",
    "tomate",
    "tonique",
    "tonneau",
    "toponyme",
    "torche",
    "tordre",
    "tornade",
    "torpille",
    "torrent",
    "torse",
    "tortue",
    "totem",
    "toucher",
    "tournage",
    "tousser",
    "toxine",
    "traction",
    "trafic",
    "tragique",
    "trahir",
    "train",
    "trancher",
    "travail",
    "trèfle",
    "tremper",
    "trésor",
    "treuil",
    "triage",
    "tribunal",
    "tricoter",
    "trilogie",
    "triomphe",
    "tripler",
    "triturer",
    "trivial",
    "trombone",
    "tronc",
    "tropical",
    "troupeau",
    "tuile",
    "tulipe",
    "tumulte",
    "tunnel",
    "turbine",
    "tuteur",
    "tutoyer",
    "tuyau",
    "tympan",
    "typhon",
    "typique",
    "tyran",
    "ubuesque",
    "ultime",
    "ultrason",
    "unanime",
    "unifier",
    "union",
    "unique",
    "unitaire",
    "univers",
    "uranium",
    "urbain",
    "urticant",
    "usage",
    "usine",
    "usuel",
    "usure",
    "utile",
    "utopie",
    "vacarme",
    "vaccin",
    "vagabond",
    "vague",
    "vaillant",
    "vaincre",
    "vaisseau",
    "valable",
    "valise",
    "vallon",
    "valve",
    "vampire",
    "vanille",
    "vapeur",
    "varier",
    "vaseux",
    "vassal",
    "vaste",
    "vecteur",
    "vedette",
    "végétal",
    "véhicule",
    "veinard",
    "véloce",
    "vendredi",
    "vénérer",
    "venger",
    "venimeux",
    "ventouse",
    "verdure",
    "vérin",
    "vernir",
    "verrou",
    "verser",
    "vertu",
    "veston",
    "vétéran",
    "vétuste",
    "vexant",
    "vexer",
    "viaduc",
    "viande",
    "victoire",
    "vidange",
    "vidéo",
    "vignette",
    "vigueur",
    "vilain",
    "village",
    "vinaigre",
    "violon",
    "vipère",
    "virement",
    "virtuose",
    "virus",
    "visage",
    "viseur",
    "vision",
    "visqueux",
    "visuel",
    "vital",
    "vitesse",
    "viticole",
    "vitrine",
    "vivace",
    "vivipare",
    "vocation",
    "voguer",
    "voile",
    "voisin",
    "voiture",
    "volaille",
    "volcan",
    "voltiger",
    "volume",
    "vorace",
    "vortex",
    "voter",
    "vouloir",
    "voyage",
    "voyelle",
    "wagon",
    "xénon",
    "yacht",
    "zèbre",
    "zénith",
    "zeste",
    "zoologie"
]

},{}],13:[function(require,module,exports){
module.exports=[
    "abaco",
    "abbaglio",
    "abbinato",
    "abete",
    "abisso",
    "abolire",
    "abrasivo",
    "abrogato",
    "accadere",
    "accenno",
    "accusato",
    "acetone",
    "achille",
    "acido",
    "acqua",
    "acre",
    "acrilico",
    "acrobata",
    "acuto",
    "adagio",
    "addebito",
    "addome",
    "adeguato",
    "aderire",
    "adipe",
    "adottare",
    "adulare",
    "affabile",
    "affetto",
    "affisso",
    "affranto",
    "aforisma",
    "afoso",
    "africano",
    "agave",
    "agente",
    "agevole",
    "aggancio",
    "agire",
    "agitare",
    "agonismo",
    "agricolo",
    "agrumeto",
    "aguzzo",
    "alabarda",
    "alato",
    "albatro",
    "alberato",
    "albo",
    "albume",
    "alce",
    "alcolico",
    "alettone",
    "alfa",
    "algebra",
    "aliante",
    "alibi",
    "alimento",
    "allagato",
    "allegro",
    "allievo",
    "allodola",
    "allusivo",
    "almeno",
    "alogeno",
    "alpaca",
    "alpestre",
    "altalena",
    "alterno",
    "alticcio",
    "altrove",
    "alunno",
    "alveolo",
    "alzare",
    "amalgama",
    "amanita",
    "amarena",
    "ambito",
    "ambrato",
    "ameba",
    "america",
    "ametista",
    "amico",
    "ammasso",
    "ammenda",
    "ammirare",
    "ammonito",
    "amore",
    "ampio",
    "ampliare",
    "amuleto",
    "anacardo",
    "anagrafe",
    "analista",
    "anarchia",
    "anatra",
    "anca",
    "ancella",
    "ancora",
    "andare",
    "andrea",
    "anello",
    "angelo",
    "angolare",
    "angusto",
    "anima",
    "annegare",
    "annidato",
    "anno",
    "annuncio",
    "anonimo",
    "anticipo",
    "anzi",
    "apatico",
    "apertura",
    "apode",
    "apparire",
    "appetito",
    "appoggio",
    "approdo",
    "appunto",
    "aprile",
    "arabica",
    "arachide",
    "aragosta",
    "araldica",
    "arancio",
    "aratura",
    "arazzo",
    "arbitro",
    "archivio",
    "ardito",
    "arenile",
    "argento",
    "argine",
    "arguto",
    "aria",
    "armonia",
    "arnese",
    "arredato",
    "arringa",
    "arrosto",
    "arsenico",
    "arso",
    "artefice",
    "arzillo",
    "asciutto",
    "ascolto",
    "asepsi",
    "asettico",
    "asfalto",
    "asino",
    "asola",
    "aspirato",
    "aspro",
    "assaggio",
    "asse",
    "assoluto",
    "assurdo",
    "asta",
    "astenuto",
    "astice",
    "astratto",
    "atavico",
    "ateismo",
    "atomico",
    "atono",
    "attesa",
    "attivare",
    "attorno",
    "attrito",
    "attuale",
    "ausilio",
    "austria",
    "autista",
    "autonomo",
    "autunno",
    "avanzato",
    "avere",
    "avvenire",
    "avviso",
    "avvolgere",
    "azione",
    "azoto",
    "azzimo",
    "azzurro",
    "babele",
    "baccano",
    "bacino",
    "baco",
    "badessa",
    "badilata",
    "bagnato",
    "baita",
    "balcone",
    "baldo",
    "balena",
    "ballata",
    "balzano",
    "bambino",
    "bandire",
    "baraonda",
    "barbaro",
    "barca",
    "baritono",
    "barlume",
    "barocco",
    "basilico",
    "basso",
    "batosta",
    "battuto",
    "baule",
    "bava",
    "bavosa",
    "becco",
    "beffa",
    "belgio",
    "belva",
    "benda",
    "benevole",
    "benigno",
    "benzina",
    "bere",
    "berlina",
    "beta",
    "bibita",
    "bici",
    "bidone",
    "bifido",
    "biga",
    "bilancia",
    "bimbo",
    "binocolo",
    "biologo",
    "bipede",
    "bipolare",
    "birbante",
    "birra",
    "biscotto",
    "bisesto",
    "bisnonno",
    "bisonte",
    "bisturi",
    "bizzarro",
    "blando",
    "blatta",
    "bollito",
    "bonifico",
    "bordo",
    "bosco",
    "botanico",
    "bottino",
    "bozzolo",
    "braccio",
    "bradipo",
    "brama",
    "branca",
    "bravura",
    "bretella",
    "brevetto",
    "brezza",
    "briglia",
    "brillante",
    "brindare",
    "broccolo",
    "brodo",
    "bronzina",
    "brullo",
    "bruno",
    "bubbone",
    "buca",
    "budino",
    "buffone",
    "buio",
    "bulbo",
    "buono",
    "burlone",
    "burrasca",
    "bussola",
    "busta",
    "cadetto",
    "caduco",
    "calamaro",
    "calcolo",
    "calesse",
    "calibro",
    "calmo",
    "caloria",
    "cambusa",
    "camerata",
    "camicia",
    "cammino",
    "camola",
    "campale",
    "canapa",
    "candela",
    "cane",
    "canino",
    "canotto",
    "cantina",
    "capace",
    "capello",
    "capitolo",
    "capogiro",
    "cappero",
    "capra",
    "capsula",
    "carapace",
    "carcassa",
    "cardo",
    "carisma",
    "carovana",
    "carretto",
    "cartolina",
    "casaccio",
    "cascata",
    "caserma",
    "caso",
    "cassone",
    "castello",
    "casuale",
    "catasta",
    "catena",
    "catrame",
    "cauto",
    "cavillo",
    "cedibile",
    "cedrata",
    "cefalo",
    "celebre",
    "cellulare",
    "cena",
    "cenone",
    "centesimo",
    "ceramica",
    "cercare",
    "certo",
    "cerume",
    "cervello",
    "cesoia",
    "cespo",
    "ceto",
    "chela",
    "chiaro",
    "chicca",
    "chiedere",
    "chimera",
    "china",
    "chirurgo",
    "chitarra",
    "ciao",
    "ciclismo",
    "cifrare",
    "cigno",
    "cilindro",
    "ciottolo",
    "circa",
    "cirrosi",
    "citrico",
    "cittadino",
    "ciuffo",
    "civetta",
    "civile",
    "classico",
    "clinica",
    "cloro",
    "cocco",
    "codardo",
    "codice",
    "coerente",
    "cognome",
    "collare",
    "colmato",
    "colore",
    "colposo",
    "coltivato",
    "colza",
    "coma",
    "cometa",
    "commando",
    "comodo",
    "computer",
    "comune",
    "conciso",
    "condurre",
    "conferma",
    "congelare",
    "coniuge",
    "connesso",
    "conoscere",
    "consumo",
    "continuo",
    "convegno",
    "coperto",
    "copione",
    "coppia",
    "copricapo",
    "corazza",
    "cordata",
    "coricato",
    "cornice",
    "corolla",
    "corpo",
    "corredo",
    "corsia",
    "cortese",
    "cosmico",
    "costante",
    "cottura",
    "covato",
    "cratere",
    "cravatta",
    "creato",
    "credere",
    "cremoso",
    "crescita",
    "creta",
    "criceto",
    "crinale",
    "crisi",
    "critico",
    "croce",
    "cronaca",
    "crostata",
    "cruciale",
    "crusca",
    "cucire",
    "cuculo",
    "cugino",
    "cullato",
    "cupola",
    "curatore",
    "cursore",
    "curvo",
    "cuscino",
    "custode",
    "dado",
    "daino",
    "dalmata",
    "damerino",
    "daniela",
    "dannoso",
    "danzare",
    "datato",
    "davanti",
    "davvero",
    "debutto",
    "decennio",
    "deciso",
    "declino",
    "decollo",
    "decreto",
    "dedicato",
    "definito",
    "deforme",
    "degno",
    "delegare",
    "delfino",
    "delirio",
    "delta",
    "demenza",
    "denotato",
    "dentro",
    "deposito",
    "derapata",
    "derivare",
    "deroga",
    "descritto",
    "deserto",
    "desiderio",
    "desumere",
    "detersivo",
    "devoto",
    "diametro",
    "dicembre",
    "diedro",
    "difeso",
    "diffuso",
    "digerire",
    "digitale",
    "diluvio",
    "dinamico",
    "dinnanzi",
    "dipinto",
    "diploma",
    "dipolo",
    "diradare",
    "dire",
    "dirotto",
    "dirupo",
    "disagio",
    "discreto",
    "disfare",
    "disgelo",
    "disposto",
    "distanza",
    "disumano",
    "dito",
    "divano",
    "divelto",
    "dividere",
    "divorato",
    "doblone",
    "docente",
    "doganale",
    "dogma",
    "dolce",
    "domato",
    "domenica",
    "dominare",
    "dondolo",
    "dono",
    "dormire",
    "dote",
    "dottore",
    "dovuto",
    "dozzina",
    "drago",
    "druido",
    "dubbio",
    "dubitare",
    "ducale",
    "duna",
    "duomo",
    "duplice",
    "duraturo",
    "ebano",
    "eccesso",
    "ecco",
    "eclissi",
    "economia",
    "edera",
    "edicola",
    "edile",
    "editoria",
    "educare",
    "egemonia",
    "egli",
    "egoismo",
    "egregio",
    "elaborato",
    "elargire",
    "elegante",
    "elencato",
    "eletto",
    "elevare",
    "elfico",
    "elica",
    "elmo",
    "elsa",
    "eluso",
    "emanato",
    "emblema",
    "emesso",
    "emiro",
    "emotivo",
    "emozione",
    "empirico",
    "emulo",
    "endemico",
    "enduro",
    "energia",
    "enfasi",
    "enoteca",
    "entrare",
    "enzima",
    "epatite",
    "epilogo",
    "episodio",
    "epocale",
    "eppure",
    "equatore",
    "erario",
    "erba",
    "erboso",
    "erede",
    "eremita",
    "erigere",
    "ermetico",
    "eroe",
    "erosivo",
    "errante",
    "esagono",
    "esame",
    "esanime",
    "esaudire",
    "esca",
    "esempio",
    "esercito",
    "esibito",
    "esigente",
    "esistere",
    "esito",
    "esofago",
    "esortato",
    "esoso",
    "espanso",
    "espresso",
    "essenza",
    "esso",
    "esteso",
    "estimare",
    "estonia",
    "estroso",
    "esultare",
    "etilico",
    "etnico",
    "etrusco",
    "etto",
    "euclideo",
    "europa",
    "evaso",
    "evidenza",
    "evitato",
    "evoluto",
    "evviva",
    "fabbrica",
    "faccenda",
    "fachiro",
    "falco",
    "famiglia",
    "fanale",
    "fanfara",
    "fango",
    "fantasma",
    "fare",
    "farfalla",
    "farinoso",
    "farmaco",
    "fascia",
    "fastoso",
    "fasullo",
    "faticare",
    "fato",
    "favoloso",
    "febbre",
    "fecola",
    "fede",
    "fegato",
    "felpa",
    "feltro",
    "femmina",
    "fendere",
    "fenomeno",
    "fermento",
    "ferro",
    "fertile",
    "fessura",
    "festivo",
    "fetta",
    "feudo",
    "fiaba",
    "fiducia",
    "fifa",
    "figurato",
    "filo",
    "finanza",
    "finestra",
    "finire",
    "fiore",
    "fiscale",
    "fisico",
    "fiume",
    "flacone",
    "flamenco",
    "flebo",
    "flemma",
    "florido",
    "fluente",
    "fluoro",
    "fobico",
    "focaccia",
    "focoso",
    "foderato",
    "foglio",
    "folata",
    "folclore",
    "folgore",
    "fondente",
    "fonetico",
    "fonia",
    "fontana",
    "forbito",
    "forchetta",
    "foresta",
    "formica",
    "fornaio",
    "foro",
    "fortezza",
    "forzare",
    "fosfato",
    "fosso",
    "fracasso",
    "frana",
    "frassino",
    "fratello",
    "freccetta",
    "frenata",
    "fresco",
    "frigo",
    "frollino",
    "fronde",
    "frugale",
    "frutta",
    "fucilata",
    "fucsia",
    "fuggente",
    "fulmine",
    "fulvo",
    "fumante",
    "fumetto",
    "fumoso",
    "fune",
    "funzione",
    "fuoco",
    "furbo",
    "furgone",
    "furore",
    "fuso",
    "futile",
    "gabbiano",
    "gaffe",
    "galateo",
    "gallina",
    "galoppo",
    "gambero",
    "gamma",
    "garanzia",
    "garbo",
    "garofano",
    "garzone",
    "gasdotto",
    "gasolio",
    "gastrico",
    "gatto",
    "gaudio",
    "gazebo",
    "gazzella",
    "geco",
    "gelatina",
    "gelso",
    "gemello",
    "gemmato",
    "gene",
    "genitore",
    "gennaio",
    "genotipo",
    "gergo",
    "ghepardo",
    "ghiaccio",
    "ghisa",
    "giallo",
    "gilda",
    "ginepro",
    "giocare",
    "gioiello",
    "giorno",
    "giove",
    "girato",
    "girone",
    "gittata",
    "giudizio",
    "giurato",
    "giusto",
    "globulo",
    "glutine",
    "gnomo",
    "gobba",
    "golf",
    "gomito",
    "gommone",
    "gonfio",
    "gonna",
    "governo",
    "gracile",
    "grado",
    "grafico",
    "grammo",
    "grande",
    "grattare",
    "gravoso",
    "grazia",
    "greca",
    "gregge",
    "grifone",
    "grigio",
    "grinza",
    "grotta",
    "gruppo",
    "guadagno",
    "guaio",
    "guanto",
    "guardare",
    "gufo",
    "guidare",
    "ibernato",
    "icona",
    "identico",
    "idillio",
    "idolo",
    "idra",
    "idrico",
    "idrogeno",
    "igiene",
    "ignaro",
    "ignorato",
    "ilare",
    "illeso",
    "illogico",
    "illudere",
    "imballo",
    "imbevuto",
    "imbocco",
    "imbuto",
    "immane",
    "immerso",
    "immolato",
    "impacco",
    "impeto",
    "impiego",
    "importo",
    "impronta",
    "inalare",
    "inarcare",
    "inattivo",
    "incanto",
    "incendio",
    "inchino",
    "incisivo",
    "incluso",
    "incontro",
    "incrocio",
    "incubo",
    "indagine",
    "india",
    "indole",
    "inedito",
    "infatti",
    "infilare",
    "inflitto",
    "ingaggio",
    "ingegno",
    "inglese",
    "ingordo",
    "ingrosso",
    "innesco",
    "inodore",
    "inoltrare",
    "inondato",
    "insano",
    "insetto",
    "insieme",
    "insonnia",
    "insulina",
    "intasato",
    "intero",
    "intonaco",
    "intuito",
    "inumidire",
    "invalido",
    "invece",
    "invito",
    "iperbole",
    "ipnotico",
    "ipotesi",
    "ippica",
    "iride",
    "irlanda",
    "ironico",
    "irrigato",
    "irrorare",
    "isolato",
    "isotopo",
    "isterico",
    "istituto",
    "istrice",
    "italia",
    "iterare",
    "labbro",
    "labirinto",
    "lacca",
    "lacerato",
    "lacrima",
    "lacuna",
    "laddove",
    "lago",
    "lampo",
    "lancetta",
    "lanterna",
    "lardoso",
    "larga",
    "laringe",
    "lastra",
    "latenza",
    "latino",
    "lattuga",
    "lavagna",
    "lavoro",
    "legale",
    "leggero",
    "lembo",
    "lentezza",
    "lenza",
    "leone",
    "lepre",
    "lesivo",
    "lessato",
    "lesto",
    "letterale",
    "leva",
    "levigato",
    "libero",
    "lido",
    "lievito",
    "lilla",
    "limatura",
    "limitare",
    "limpido",
    "lineare",
    "lingua",
    "liquido",
    "lira",
    "lirica",
    "lisca",
    "lite",
    "litigio",
    "livrea",
    "locanda",
    "lode",
    "logica",
    "lombare",
    "londra",
    "longevo",
    "loquace",
    "lorenzo",
    "loto",
    "lotteria",
    "luce",
    "lucidato",
    "lumaca",
    "luminoso",
    "lungo",
    "lupo",
    "luppolo",
    "lusinga",
    "lusso",
    "lutto",
    "macabro",
    "macchina",
    "macero",
    "macinato",
    "madama",
    "magico",
    "maglia",
    "magnete",
    "magro",
    "maiolica",
    "malafede",
    "malgrado",
    "malinteso",
    "malsano",
    "malto",
    "malumore",
    "mana",
    "mancia",
    "mandorla",
    "mangiare",
    "manifesto",
    "mannaro",
    "manovra",
    "mansarda",
    "mantide",
    "manubrio",
    "mappa",
    "maratona",
    "marcire",
    "maretta",
    "marmo",
    "marsupio",
    "maschera",
    "massaia",
    "mastino",
    "materasso",
    "matricola",
    "mattone",
    "maturo",
    "mazurca",
    "meandro",
    "meccanico",
    "mecenate",
    "medesimo",
    "meditare",
    "mega",
    "melassa",
    "melis",
    "melodia",
    "meninge",
    "meno",
    "mensola",
    "mercurio",
    "merenda",
    "merlo",
    "meschino",
    "mese",
    "messere",
    "mestolo",
    "metallo",
    "metodo",
    "mettere",
    "miagolare",
    "mica",
    "micelio",
    "michele",
    "microbo",
    "midollo",
    "miele",
    "migliore",
    "milano",
    "milite",
    "mimosa",
    "minerale",
    "mini",
    "minore",
    "mirino",
    "mirtillo",
    "miscela",
    "missiva",
    "misto",
    "misurare",
    "mitezza",
    "mitigare",
    "mitra",
    "mittente",
    "mnemonico",
    "modello",
    "modifica",
    "modulo",
    "mogano",
    "mogio",
    "mole",
    "molosso",
    "monastero",
    "monco",
    "mondina",
    "monetario",
    "monile",
    "monotono",
    "monsone",
    "montato",
    "monviso",
    "mora",
    "mordere",
    "morsicato",
    "mostro",
    "motivato",
    "motosega",
    "motto",
    "movenza",
    "movimento",
    "mozzo",
    "mucca",
    "mucosa",
    "muffa",
    "mughetto",
    "mugnaio",
    "mulatto",
    "mulinello",
    "multiplo",
    "mummia",
    "munto",
    "muovere",
    "murale",
    "musa",
    "muscolo",
    "musica",
    "mutevole",
    "muto",
    "nababbo",
    "nafta",
    "nanometro",
    "narciso",
    "narice",
    "narrato",
    "nascere",
    "nastrare",
    "naturale",
    "nautica",
    "naviglio",
    "nebulosa",
    "necrosi",
    "negativo",
    "negozio",
    "nemmeno",
    "neofita",
    "neretto",
    "nervo",
    "nessuno",
    "nettuno",
    "neutrale",
    "neve",
    "nevrotico",
    "nicchia",
    "ninfa",
    "nitido",
    "nobile",
    "nocivo",
    "nodo",
    "nome",
    "nomina",
    "nordico",
    "normale",
    "norvegese",
    "nostrano",
    "notare",
    "notizia",
    "notturno",
    "novella",
    "nucleo",
    "nulla",
    "numero",
    "nuovo",
    "nutrire",
    "nuvola",
    "nuziale",
    "oasi",
    "obbedire",
    "obbligo",
    "obelisco",
    "oblio",
    "obolo",
    "obsoleto",
    "occasione",
    "occhio",
    "occidente",
    "occorrere",
    "occultare",
    "ocra",
    "oculato",
    "odierno",
    "odorare",
    "offerta",
    "offrire",
    "offuscato",
    "oggetto",
    "oggi",
    "ognuno",
    "olandese",
    "olfatto",
    "oliato",
    "oliva",
    "ologramma",
    "oltre",
    "omaggio",
    "ombelico",
    "ombra",
    "omega",
    "omissione",
    "ondoso",
    "onere",
    "onice",
    "onnivoro",
    "onorevole",
    "onta",
    "operato",
    "opinione",
    "opposto",
    "oracolo",
    "orafo",
    "ordine",
    "orecchino",
    "orefice",
    "orfano",
    "organico",
    "origine",
    "orizzonte",
    "orma",
    "ormeggio",
    "ornativo",
    "orologio",
    "orrendo",
    "orribile",
    "ortensia",
    "ortica",
    "orzata",
    "orzo",
    "osare",
    "oscurare",
    "osmosi",
    "ospedale",
    "ospite",
    "ossa",
    "ossidare",
    "ostacolo",
    "oste",
    "otite",
    "otre",
    "ottagono",
    "ottimo",
    "ottobre",
    "ovale",
    "ovest",
    "ovino",
    "oviparo",
    "ovocito",
    "ovunque",
    "ovviare",
    "ozio",
    "pacchetto",
    "pace",
    "pacifico",
    "padella",
    "padrone",
    "paese",
    "paga",
    "pagina",
    "palazzina",
    "palesare",
    "pallido",
    "palo",
    "palude",
    "pandoro",
    "pannello",
    "paolo",
    "paonazzo",
    "paprica",
    "parabola",
    "parcella",
    "parere",
    "pargolo",
    "pari",
    "parlato",
    "parola",
    "partire",
    "parvenza",
    "parziale",
    "passivo",
    "pasticca",
    "patacca",
    "patologia",
    "pattume",
    "pavone",
    "peccato",
    "pedalare",
    "pedonale",
    "peggio",
    "peloso",
    "penare",
    "pendice",
    "penisola",
    "pennuto",
    "penombra",
    "pensare",
    "pentola",
    "pepe",
    "pepita",
    "perbene",
    "percorso",
    "perdonato",
    "perforare",
    "pergamena",
    "periodo",
    "permesso",
    "perno",
    "perplesso",
    "persuaso",
    "pertugio",
    "pervaso",
    "pesatore",
    "pesista",
    "peso",
    "pestifero",
    "petalo",
    "pettine",
    "petulante",
    "pezzo",
    "piacere",
    "pianta",
    "piattino",
    "piccino",
    "picozza",
    "piega",
    "pietra",
    "piffero",
    "pigiama",
    "pigolio",
    "pigro",
    "pila",
    "pilifero",
    "pillola",
    "pilota",
    "pimpante",
    "pineta",
    "pinna",
    "pinolo",
    "pioggia",
    "piombo",
    "piramide",
    "piretico",
    "pirite",
    "pirolisi",
    "pitone",
    "pizzico",
    "placebo",
    "planare",
    "plasma",
    "platano",
    "plenario",
    "pochezza",
    "poderoso",
    "podismo",
    "poesia",
    "poggiare",
    "polenta",
    "poligono",
    "pollice",
    "polmonite",
    "polpetta",
    "polso",
    "poltrona",
    "polvere",
    "pomice",
    "pomodoro",
    "ponte",
    "popoloso",
    "porfido",
    "poroso",
    "porpora",
    "porre",
    "portata",
    "posa",
    "positivo",
    "possesso",
    "postulato",
    "potassio",
    "potere",
    "pranzo",
    "prassi",
    "pratica",
    "precluso",
    "predica",
    "prefisso",
    "pregiato",
    "prelievo",
    "premere",
    "prenotare",
    "preparato",
    "presenza",
    "pretesto",
    "prevalso",
    "prima",
    "principe",
    "privato",
    "problema",
    "procura",
    "produrre",
    "profumo",
    "progetto",
    "prolunga",
    "promessa",
    "pronome",
    "proposta",
    "proroga",
    "proteso",
    "prova",
    "prudente",
    "prugna",
    "prurito",
    "psiche",
    "pubblico",
    "pudica",
    "pugilato",
    "pugno",
    "pulce",
    "pulito",
    "pulsante",
    "puntare",
    "pupazzo",
    "pupilla",
    "puro",
    "quadro",
    "qualcosa",
    "quasi",
    "querela",
    "quota",
    "raccolto",
    "raddoppio",
    "radicale",
    "radunato",
    "raffica",
    "ragazzo",
    "ragione",
    "ragno",
    "ramarro",
    "ramingo",
    "ramo",
    "randagio",
    "rantolare",
    "rapato",
    "rapina",
    "rappreso",
    "rasatura",
    "raschiato",
    "rasente",
    "rassegna",
    "rastrello",
    "rata",
    "ravveduto",
    "reale",
    "recepire",
    "recinto",
    "recluta",
    "recondito",
    "recupero",
    "reddito",
    "redimere",
    "regalato",
    "registro",
    "regola",
    "regresso",
    "relazione",
    "remare",
    "remoto",
    "renna",
    "replica",
    "reprimere",
    "reputare",
    "resa",
    "residente",
    "responso",
    "restauro",
    "rete",
    "retina",
    "retorica",
    "rettifica",
    "revocato",
    "riassunto",
    "ribadire",
    "ribelle",
    "ribrezzo",
    "ricarica",
    "ricco",
    "ricevere",
    "riciclato",
    "ricordo",
    "ricreduto",
    "ridicolo",
    "ridurre",
    "rifasare",
    "riflesso",
    "riforma",
    "rifugio",
    "rigare",
    "rigettato",
    "righello",
    "rilassato",
    "rilevato",
    "rimanere",
    "rimbalzo",
    "rimedio",
    "rimorchio",
    "rinascita",
    "rincaro",
    "rinforzo",
    "rinnovo",
    "rinomato",
    "rinsavito",
    "rintocco",
    "rinuncia",
    "rinvenire",
    "riparato",
    "ripetuto",
    "ripieno",
    "riportare",
    "ripresa",
    "ripulire",
    "risata",
    "rischio",
    "riserva",
    "risibile",
    "riso",
    "rispetto",
    "ristoro",
    "risultato",
    "risvolto",
    "ritardo",
    "ritegno",
    "ritmico",
    "ritrovo",
    "riunione",
    "riva",
    "riverso",
    "rivincita",
    "rivolto",
    "rizoma",
    "roba",
    "robotico",
    "robusto",
    "roccia",
    "roco",
    "rodaggio",
    "rodere",
    "roditore",
    "rogito",
    "rollio",
    "romantico",
    "rompere",
    "ronzio",
    "rosolare",
    "rospo",
    "rotante",
    "rotondo",
    "rotula",
    "rovescio",
    "rubizzo",
    "rubrica",
    "ruga",
    "rullino",
    "rumine",
    "rumoroso",
    "ruolo",
    "rupe",
    "russare",
    "rustico",
    "sabato",
    "sabbiare",
    "sabotato",
    "sagoma",
    "salasso",
    "saldatura",
    "salgemma",
    "salivare",
    "salmone",
    "salone",
    "saltare",
    "saluto",
    "salvo",
    "sapere",
    "sapido",
    "saporito",
    "saraceno",
    "sarcasmo",
    "sarto",
    "sassoso",
    "satellite",
    "satira",
    "satollo",
    "saturno",
    "savana",
    "savio",
    "saziato",
    "sbadiglio",
    "sbalzo",
    "sbancato",
    "sbarra",
    "sbattere",
    "sbavare",
    "sbendare",
    "sbirciare",
    "sbloccato",
    "sbocciato",
    "sbrinare",
    "sbruffone",
    "sbuffare",
    "scabroso",
    "scadenza",
    "scala",
    "scambiare",
    "scandalo",
    "scapola",
    "scarso",
    "scatenare",
    "scavato",
    "scelto",
    "scenico",
    "scettro",
    "scheda",
    "schiena",
    "sciarpa",
    "scienza",
    "scindere",
    "scippo",
    "sciroppo",
    "scivolo",
    "sclerare",
    "scodella",
    "scolpito",
    "scomparto",
    "sconforto",
    "scoprire",
    "scorta",
    "scossone",
    "scozzese",
    "scriba",
    "scrollare",
    "scrutinio",
    "scuderia",
    "scultore",
    "scuola",
    "scuro",
    "scusare",
    "sdebitare",
    "sdoganare",
    "seccatura",
    "secondo",
    "sedano",
    "seggiola",
    "segnalato",
    "segregato",
    "seguito",
    "selciato",
    "selettivo",
    "sella",
    "selvaggio",
    "semaforo",
    "sembrare",
    "seme",
    "seminato",
    "sempre",
    "senso",
    "sentire",
    "sepolto",
    "sequenza",
    "serata",
    "serbato",
    "sereno",
    "serio",
    "serpente",
    "serraglio",
    "servire",
    "sestina",
    "setola",
    "settimana",
    "sfacelo",
    "sfaldare",
    "sfamato",
    "sfarzoso",
    "sfaticato",
    "sfera",
    "sfida",
    "sfilato",
    "sfinge",
    "sfocato",
    "sfoderare",
    "sfogo",
    "sfoltire",
    "sforzato",
    "sfratto",
    "sfruttato",
    "sfuggito",
    "sfumare",
    "sfuso",
    "sgabello",
    "sgarbato",
    "sgonfiare",
    "sgorbio",
    "sgrassato",
    "sguardo",
    "sibilo",
    "siccome",
    "sierra",
    "sigla",
    "signore",
    "silenzio",
    "sillaba",
    "simbolo",
    "simpatico",
    "simulato",
    "sinfonia",
    "singolo",
    "sinistro",
    "sino",
    "sintesi",
    "sinusoide",
    "sipario",
    "sisma",
    "sistole",
    "situato",
    "slitta",
    "slogatura",
    "sloveno",
    "smarrito",
    "smemorato",
    "smentito",
    "smeraldo",
    "smilzo",
    "smontare",
    "smottato",
    "smussato",
    "snellire",
    "snervato",
    "snodo",
    "sobbalzo",
    "sobrio",
    "soccorso",
    "sociale",
    "sodale",
    "soffitto",
    "sogno",
    "soldato",
    "solenne",
    "solido",
    "sollazzo",
    "solo",
    "solubile",
    "solvente",
    "somatico",
    "somma",
    "sonda",
    "sonetto",
    "sonnifero",
    "sopire",
    "soppeso",
    "sopra",
    "sorgere",
    "sorpasso",
    "sorriso",
    "sorso",
    "sorteggio",
    "sorvolato",
    "sospiro",
    "sosta",
    "sottile",
    "spada",
    "spalla",
    "spargere",
    "spatola",
    "spavento",
    "spazzola",
    "specie",
    "spedire",
    "spegnere",
    "spelatura",
    "speranza",
    "spessore",
    "spettrale",
    "spezzato",
    "spia",
    "spigoloso",
    "spillato",
    "spinoso",
    "spirale",
    "splendido",
    "sportivo",
    "sposo",
    "spranga",
    "sprecare",
    "spronato",
    "spruzzo",
    "spuntino",
    "squillo",
    "sradicare",
    "srotolato",
    "stabile",
    "stacco",
    "staffa",
    "stagnare",
    "stampato",
    "stantio",
    "starnuto",
    "stasera",
    "statuto",
    "stelo",
    "steppa",
    "sterzo",
    "stiletto",
    "stima",
    "stirpe",
    "stivale",
    "stizzoso",
    "stonato",
    "storico",
    "strappo",
    "stregato",
    "stridulo",
    "strozzare",
    "strutto",
    "stuccare",
    "stufo",
    "stupendo",
    "subentro",
    "succoso",
    "sudore",
    "suggerito",
    "sugo",
    "sultano",
    "suonare",
    "superbo",
    "supporto",
    "surgelato",
    "surrogato",
    "sussurro",
    "sutura",
    "svagare",
    "svedese",
    "sveglio",
    "svelare",
    "svenuto",
    "svezia",
    "sviluppo",
    "svista",
    "svizzera",
    "svolta",
    "svuotare",
    "tabacco",
    "tabulato",
    "tacciare",
    "taciturno",
    "tale",
    "talismano",
    "tampone",
    "tannino",
    "tara",
    "tardivo",
    "targato",
    "tariffa",
    "tarpare",
    "tartaruga",
    "tasto",
    "tattico",
    "taverna",
    "tavolata",
    "tazza",
    "teca",
    "tecnico",
    "telefono",
    "temerario",
    "tempo",
    "temuto",
    "tendone",
    "tenero",
    "tensione",
    "tentacolo",
    "teorema",
    "terme",
    "terrazzo",
    "terzetto",
    "tesi",
    "tesserato",
    "testato",
    "tetro",
    "tettoia",
    "tifare",
    "tigella",
    "timbro",
    "tinto",
    "tipico",
    "tipografo",
    "tiraggio",
    "tiro",
    "titanio",
    "titolo",
    "titubante",
    "tizio",
    "tizzone",
    "toccare",
    "tollerare",
    "tolto",
    "tombola",
    "tomo",
    "tonfo",
    "tonsilla",
    "topazio",
    "topologia",
    "toppa",
    "torba",
    "tornare",
    "torrone",
    "tortora",
    "toscano",
    "tossire",
    "tostatura",
    "totano",
    "trabocco",
    "trachea",
    "trafila",
    "tragedia",
    "tralcio",
    "tramonto",
    "transito",
    "trapano",
    "trarre",
    "trasloco",
    "trattato",
    "trave",
    "treccia",
    "tremolio",
    "trespolo",
    "tributo",
    "tricheco",
    "trifoglio",
    "trillo",
    "trincea",
    "trio",
    "tristezza",
    "triturato",
    "trivella",
    "tromba",
    "trono",
    "troppo",
    "trottola",
    "trovare",
    "truccato",
    "tubatura",
    "tuffato",
    "tulipano",
    "tumulto",
    "tunisia",
    "turbare",
    "turchino",
    "tuta",
    "tutela",
    "ubicato",
    "uccello",
    "uccisore",
    "udire",
    "uditivo",
    "uffa",
    "ufficio",
    "uguale",
    "ulisse",
    "ultimato",
    "umano",
    "umile",
    "umorismo",
    "uncinetto",
    "ungere",
    "ungherese",
    "unicorno",
    "unificato",
    "unisono",
    "unitario",
    "unte",
    "uovo",
    "upupa",
    "uragano",
    "urgenza",
    "urlo",
    "usanza",
    "usato",
    "uscito",
    "usignolo",
    "usuraio",
    "utensile",
    "utilizzo",
    "utopia",
    "vacante",
    "vaccinato",
    "vagabondo",
    "vagliato",
    "valanga",
    "valgo",
    "valico",
    "valletta",
    "valoroso",
    "valutare",
    "valvola",
    "vampata",
    "vangare",
    "vanitoso",
    "vano",
    "vantaggio",
    "vanvera",
    "vapore",
    "varano",
    "varcato",
    "variante",
    "vasca",
    "vedetta",
    "vedova",
    "veduto",
    "vegetale",
    "veicolo",
    "velcro",
    "velina",
    "velluto",
    "veloce",
    "venato",
    "vendemmia",
    "vento",
    "verace",
    "verbale",
    "vergogna",
    "verifica",
    "vero",
    "verruca",
    "verticale",
    "vescica",
    "vessillo",
    "vestale",
    "veterano",
    "vetrina",
    "vetusto",
    "viandante",
    "vibrante",
    "vicenda",
    "vichingo",
    "vicinanza",
    "vidimare",
    "vigilia",
    "vigneto",
    "vigore",
    "vile",
    "villano",
    "vimini",
    "vincitore",
    "viola",
    "vipera",
    "virgola",
    "virologo",
    "virulento",
    "viscoso",
    "visione",
    "vispo",
    "vissuto",
    "visura",
    "vita",
    "vitello",
    "vittima",
    "vivanda",
    "vivido",
    "viziare",
    "voce",
    "voga",
    "volatile",
    "volere",
    "volpe",
    "voragine",
    "vulcano",
    "zampogna",
    "zanna",
    "zappato",
    "zattera",
    "zavorra",
    "zefiro",
    "zelante",
    "zelo",
    "zenzero",
    "zerbino",
    "zibetto",
    "zinco",
    "zircone",
    "zitto",
    "zolla",
    "zotico",
    "zucchero",
    "zufolo",
    "zulu",
    "zuppa"
]

},{}],14:[function(require,module,exports){
module.exports=[
    "あいこくしん",
    "あいさつ",
    "あいだ",
    "あおぞら",
    "あかちゃん",
    "あきる",
    "あけがた",
    "あける",
    "あこがれる",
    "あさい",
    "あさひ",
    "あしあと",
    "あじわう",
    "あずかる",
    "あずき",
    "あそぶ",
    "あたえる",
    "あたためる",
    "あたりまえ",
    "あたる",
    "あつい",
    "あつかう",
    "あっしゅく",
    "あつまり",
    "あつめる",
    "あてな",
    "あてはまる",
    "あひる",
    "あぶら",
    "あぶる",
    "あふれる",
    "あまい",
    "あまど",
    "あまやかす",
    "あまり",
    "あみもの",
    "あめりか",
    "あやまる",
    "あゆむ",
    "あらいぐま",
    "あらし",
    "あらすじ",
    "あらためる",
    "あらゆる",
    "あらわす",
    "ありがとう",
    "あわせる",
    "あわてる",
    "あんい",
    "あんがい",
    "あんこ",
    "あんぜん",
    "あんてい",
    "あんない",
    "あんまり",
    "いいだす",
    "いおん",
    "いがい",
    "いがく",
    "いきおい",
    "いきなり",
    "いきもの",
    "いきる",
    "いくじ",
    "いくぶん",
    "いけばな",
    "いけん",
    "いこう",
    "いこく",
    "いこつ",
    "いさましい",
    "いさん",
    "いしき",
    "いじゅう",
    "いじょう",
    "いじわる",
    "いずみ",
    "いずれ",
    "いせい",
    "いせえび",
    "いせかい",
    "いせき",
    "いぜん",
    "いそうろう",
    "いそがしい",
    "いだい",
    "いだく",
    "いたずら",
    "いたみ",
    "いたりあ",
    "いちおう",
    "いちじ",
    "いちど",
    "いちば",
    "いちぶ",
    "いちりゅう",
    "いつか",
    "いっしゅん",
    "いっせい",
    "いっそう",
    "いったん",
    "いっち",
    "いってい",
    "いっぽう",
    "いてざ",
    "いてん",
    "いどう",
    "いとこ",
    "いない",
    "いなか",
    "いねむり",
    "いのち",
    "いのる",
    "いはつ",
    "いばる",
    "いはん",
    "いびき",
    "いひん",
    "いふく",
    "いへん",
    "いほう",
    "いみん",
    "いもうと",
    "いもたれ",
    "いもり",
    "いやがる",
    "いやす",
    "いよかん",
    "いよく",
    "いらい",
    "いらすと",
    "いりぐち",
    "いりょう",
    "いれい",
    "いれもの",
    "いれる",
    "いろえんぴつ",
    "いわい",
    "いわう",
    "いわかん",
    "いわば",
    "いわゆる",
    "いんげんまめ",
    "いんさつ",
    "いんしょう",
    "いんよう",
    "うえき",
    "うえる",
    "うおざ",
    "うがい",
    "うかぶ",
    "うかべる",
    "うきわ",
    "うくらいな",
    "うくれれ",
    "うけたまわる",
    "うけつけ",
    "うけとる",
    "うけもつ",
    "うける",
    "うごかす",
    "うごく",
    "うこん",
    "うさぎ",
    "うしなう",
    "うしろがみ",
    "うすい",
    "うすぎ",
    "うすぐらい",
    "うすめる",
    "うせつ",
    "うちあわせ",
    "うちがわ",
    "うちき",
    "うちゅう",
    "うっかり",
    "うつくしい",
    "うったえる",
    "うつる",
    "うどん",
    "うなぎ",
    "うなじ",
    "うなずく",
    "うなる",
    "うねる",
    "うのう",
    "うぶげ",
    "うぶごえ",
    "うまれる",
    "うめる",
    "うもう",
    "うやまう",
    "うよく",
    "うらがえす",
    "うらぐち",
    "うらない",
    "うりあげ",
    "うりきれ",
    "うるさい",
    "うれしい",
    "うれゆき",
    "うれる",
    "うろこ",
    "うわき",
    "うわさ",
    "うんこう",
    "うんちん",
    "うんてん",
    "うんどう",
    "えいえん",
    "えいが",
    "えいきょう",
    "えいご",
    "えいせい",
    "えいぶん",
    "えいよう",
    "えいわ",
    "えおり",
    "えがお",
    "えがく",
    "えきたい",
    "えくせる",
    "えしゃく",
    "えすて",
    "えつらん",
    "えのぐ",
    "えほうまき",
    "えほん",
    "えまき",
    "えもじ",
    "えもの",
    "えらい",
    "えらぶ",
    "えりあ",
    "えんえん",
    "えんかい",
    "えんぎ",
    "えんげき",
    "えんしゅう",
    "えんぜつ",
    "えんそく",
    "えんちょう",
    "えんとつ",
    "おいかける",
    "おいこす",
    "おいしい",
    "おいつく",
    "おうえん",
    "おうさま",
    "おうじ",
    "おうせつ",
    "おうたい",
    "おうふく",
    "おうべい",
    "おうよう",
    "おえる",
    "おおい",
    "おおう",
    "おおどおり",
    "おおや",
    "おおよそ",
    "おかえり",
    "おかず",
    "おがむ",
    "おかわり",
    "おぎなう",
    "おきる",
    "おくさま",
    "おくじょう",
    "おくりがな",
    "おくる",
    "おくれる",
    "おこす",
    "おこなう",
    "おこる",
    "おさえる",
    "おさない",
    "おさめる",
    "おしいれ",
    "おしえる",
    "おじぎ",
    "おじさん",
    "おしゃれ",
    "おそらく",
    "おそわる",
    "おたがい",
    "おたく",
    "おだやか",
    "おちつく",
    "おっと",
    "おつり",
    "おでかけ",
    "おとしもの",
    "おとなしい",
    "おどり",
    "おどろかす",
    "おばさん",
    "おまいり",
    "おめでとう",
    "おもいで",
    "おもう",
    "おもたい",
    "おもちゃ",
    "おやつ",
    "おやゆび",
    "およぼす",
    "おらんだ",
    "おろす",
    "おんがく",
    "おんけい",
    "おんしゃ",
    "おんせん",
    "おんだん",
    "おんちゅう",
    "おんどけい",
    "かあつ",
    "かいが",
    "がいき",
    "がいけん",
    "がいこう",
    "かいさつ",
    "かいしゃ",
    "かいすいよく",
    "かいぜん",
    "かいぞうど",
    "かいつう",
    "かいてん",
    "かいとう",
    "かいふく",
    "がいへき",
    "かいほう",
    "かいよう",
    "がいらい",
    "かいわ",
    "かえる",
    "かおり",
    "かかえる",
    "かがく",
    "かがし",
    "かがみ",
    "かくご",
    "かくとく",
    "かざる",
    "がぞう",
    "かたい",
    "かたち",
    "がちょう",
    "がっきゅう",
    "がっこう",
    "がっさん",
    "がっしょう",
    "かなざわし",
    "かのう",
    "がはく",
    "かぶか",
    "かほう",
    "かほご",
    "かまう",
    "かまぼこ",
    "かめれおん",
    "かゆい",
    "かようび",
    "からい",
    "かるい",
    "かろう",
    "かわく",
    "かわら",
    "がんか",
    "かんけい",
    "かんこう",
    "かんしゃ",
    "かんそう",
    "かんたん",
    "かんち",
    "がんばる",
    "きあい",
    "きあつ",
    "きいろ",
    "ぎいん",
    "きうい",
    "きうん",
    "きえる",
    "きおう",
    "きおく",
    "きおち",
    "きおん",
    "きかい",
    "きかく",
    "きかんしゃ",
    "ききて",
    "きくばり",
    "きくらげ",
    "きけんせい",
    "きこう",
    "きこえる",
    "きこく",
    "きさい",
    "きさく",
    "きさま",
    "きさらぎ",
    "ぎじかがく",
    "ぎしき",
    "ぎじたいけん",
    "ぎじにってい",
    "ぎじゅつしゃ",
    "きすう",
    "きせい",
    "きせき",
    "きせつ",
    "きそう",
    "きぞく",
    "きぞん",
    "きたえる",
    "きちょう",
    "きつえん",
    "ぎっちり",
    "きつつき",
    "きつね",
    "きてい",
    "きどう",
    "きどく",
    "きない",
    "きなが",
    "きなこ",
    "きぬごし",
    "きねん",
    "きのう",
    "きのした",
    "きはく",
    "きびしい",
    "きひん",
    "きふく",
    "きぶん",
    "きぼう",
    "きほん",
    "きまる",
    "きみつ",
    "きむずかしい",
    "きめる",
    "きもだめし",
    "きもち",
    "きもの",
    "きゃく",
    "きやく",
    "ぎゅうにく",
    "きよう",
    "きょうりゅう",
    "きらい",
    "きらく",
    "きりん",
    "きれい",
    "きれつ",
    "きろく",
    "ぎろん",
    "きわめる",
    "ぎんいろ",
    "きんかくじ",
    "きんじょ",
    "きんようび",
    "ぐあい",
    "くいず",
    "くうかん",
    "くうき",
    "くうぐん",
    "くうこう",
    "ぐうせい",
    "くうそう",
    "ぐうたら",
    "くうふく",
    "くうぼ",
    "くかん",
    "くきょう",
    "くげん",
    "ぐこう",
    "くさい",
    "くさき",
    "くさばな",
    "くさる",
    "くしゃみ",
    "くしょう",
    "くすのき",
    "くすりゆび",
    "くせげ",
    "くせん",
    "ぐたいてき",
    "くださる",
    "くたびれる",
    "くちこみ",
    "くちさき",
    "くつした",
    "ぐっすり",
    "くつろぐ",
    "くとうてん",
    "くどく",
    "くなん",
    "くねくね",
    "くのう",
    "くふう",
    "くみあわせ",
    "くみたてる",
    "くめる",
    "くやくしょ",
    "くらす",
    "くらべる",
    "くるま",
    "くれる",
    "くろう",
    "くわしい",
    "ぐんかん",
    "ぐんしょく",
    "ぐんたい",
    "ぐんて",
    "けあな",
    "けいかく",
    "けいけん",
    "けいこ",
    "けいさつ",
    "げいじゅつ",
    "けいたい",
    "げいのうじん",
    "けいれき",
    "けいろ",
    "けおとす",
    "けおりもの",
    "げきか",
    "げきげん",
    "げきだん",
    "げきちん",
    "げきとつ",
    "げきは",
    "げきやく",
    "げこう",
    "げこくじょう",
    "げざい",
    "けさき",
    "げざん",
    "けしき",
    "けしごむ",
    "けしょう",
    "げすと",
    "けたば",
    "けちゃっぷ",
    "けちらす",
    "けつあつ",
    "けつい",
    "けつえき",
    "けっこん",
    "けつじょ",
    "けっせき",
    "けってい",
    "けつまつ",
    "げつようび",
    "げつれい",
    "けつろん",
    "げどく",
    "けとばす",
    "けとる",
    "けなげ",
    "けなす",
    "けなみ",
    "けぬき",
    "げねつ",
    "けねん",
    "けはい",
    "げひん",
    "けぶかい",
    "げぼく",
    "けまり",
    "けみかる",
    "けむし",
    "けむり",
    "けもの",
    "けらい",
    "けろけろ",
    "けわしい",
    "けんい",
    "けんえつ",
    "けんお",
    "けんか",
    "げんき",
    "けんげん",
    "けんこう",
    "けんさく",
    "けんしゅう",
    "けんすう",
    "げんそう",
    "けんちく",
    "けんてい",
    "けんとう",
    "けんない",
    "けんにん",
    "げんぶつ",
    "けんま",
    "けんみん",
    "けんめい",
    "けんらん",
    "けんり",
    "こあくま",
    "こいぬ",
    "こいびと",
    "ごうい",
    "こうえん",
    "こうおん",
    "こうかん",
    "ごうきゅう",
    "ごうけい",
    "こうこう",
    "こうさい",
    "こうじ",
    "こうすい",
    "ごうせい",
    "こうそく",
    "こうたい",
    "こうちゃ",
    "こうつう",
    "こうてい",
    "こうどう",
    "こうない",
    "こうはい",
    "ごうほう",
    "ごうまん",
    "こうもく",
    "こうりつ",
    "こえる",
    "こおり",
    "ごかい",
    "ごがつ",
    "ごかん",
    "こくご",
    "こくさい",
    "こくとう",
    "こくない",
    "こくはく",
    "こぐま",
    "こけい",
    "こける",
    "ここのか",
    "こころ",
    "こさめ",
    "こしつ",
    "こすう",
    "こせい",
    "こせき",
    "こぜん",
    "こそだて",
    "こたい",
    "こたえる",
    "こたつ",
    "こちょう",
    "こっか",
    "こつこつ",
    "こつばん",
    "こつぶ",
    "こてい",
    "こてん",
    "ことがら",
    "ことし",
    "ことば",
    "ことり",
    "こなごな",
    "こねこね",
    "このまま",
    "このみ",
    "このよ",
    "ごはん",
    "こひつじ",
    "こふう",
    "こふん",
    "こぼれる",
    "ごまあぶら",
    "こまかい",
    "ごますり",
    "こまつな",
    "こまる",
    "こむぎこ",
    "こもじ",
    "こもち",
    "こもの",
    "こもん",
    "こやく",
    "こやま",
    "こゆう",
    "こゆび",
    "こよい",
    "こよう",
    "こりる",
    "これくしょん",
    "ころっけ",
    "こわもて",
    "こわれる",
    "こんいん",
    "こんかい",
    "こんき",
    "こんしゅう",
    "こんすい",
    "こんだて",
    "こんとん",
    "こんなん",
    "こんびに",
    "こんぽん",
    "こんまけ",
    "こんや",
    "こんれい",
    "こんわく",
    "ざいえき",
    "さいかい",
    "さいきん",
    "ざいげん",
    "ざいこ",
    "さいしょ",
    "さいせい",
    "ざいたく",
    "ざいちゅう",
    "さいてき",
    "ざいりょう",
    "さうな",
    "さかいし",
    "さがす",
    "さかな",
    "さかみち",
    "さがる",
    "さぎょう",
    "さくし",
    "さくひん",
    "さくら",
    "さこく",
    "さこつ",
    "さずかる",
    "ざせき",
    "さたん",
    "さつえい",
    "ざつおん",
    "ざっか",
    "ざつがく",
    "さっきょく",
    "ざっし",
    "さつじん",
    "ざっそう",
    "さつたば",
    "さつまいも",
    "さてい",
    "さといも",
    "さとう",
    "さとおや",
    "さとし",
    "さとる",
    "さのう",
    "さばく",
    "さびしい",
    "さべつ",
    "さほう",
    "さほど",
    "さます",
    "さみしい",
    "さみだれ",
    "さむけ",
    "さめる",
    "さやえんどう",
    "さゆう",
    "さよう",
    "さよく",
    "さらだ",
    "ざるそば",
    "さわやか",
    "さわる",
    "さんいん",
    "さんか",
    "さんきゃく",
    "さんこう",
    "さんさい",
    "ざんしょ",
    "さんすう",
    "さんせい",
    "さんそ",
    "さんち",
    "さんま",
    "さんみ",
    "さんらん",
    "しあい",
    "しあげ",
    "しあさって",
    "しあわせ",
    "しいく",
    "しいん",
    "しうち",
    "しえい",
    "しおけ",
    "しかい",
    "しかく",
    "じかん",
    "しごと",
    "しすう",
    "じだい",
    "したうけ",
    "したぎ",
    "したて",
    "したみ",
    "しちょう",
    "しちりん",
    "しっかり",
    "しつじ",
    "しつもん",
    "してい",
    "してき",
    "してつ",
    "じてん",
    "じどう",
    "しなぎれ",
    "しなもの",
    "しなん",
    "しねま",
    "しねん",
    "しのぐ",
    "しのぶ",
    "しはい",
    "しばかり",
    "しはつ",
    "しはらい",
    "しはん",
    "しひょう",
    "しふく",
    "じぶん",
    "しへい",
    "しほう",
    "しほん",
    "しまう",
    "しまる",
    "しみん",
    "しむける",
    "じむしょ",
    "しめい",
    "しめる",
    "しもん",
    "しゃいん",
    "しゃうん",
    "しゃおん",
    "じゃがいも",
    "しやくしょ",
    "しゃくほう",
    "しゃけん",
    "しゃこ",
    "しゃざい",
    "しゃしん",
    "しゃせん",
    "しゃそう",
    "しゃたい",
    "しゃちょう",
    "しゃっきん",
    "じゃま",
    "しゃりん",
    "しゃれい",
    "じゆう",
    "じゅうしょ",
    "しゅくはく",
    "じゅしん",
    "しゅっせき",
    "しゅみ",
    "しゅらば",
    "じゅんばん",
    "しょうかい",
    "しょくたく",
    "しょっけん",
    "しょどう",
    "しょもつ",
    "しらせる",
    "しらべる",
    "しんか",
    "しんこう",
    "じんじゃ",
    "しんせいじ",
    "しんちく",
    "しんりん",
    "すあげ",
    "すあし",
    "すあな",
    "ずあん",
    "すいえい",
    "すいか",
    "すいとう",
    "ずいぶん",
    "すいようび",
    "すうがく",
    "すうじつ",
    "すうせん",
    "すおどり",
    "すきま",
    "すくう",
    "すくない",
    "すける",
    "すごい",
    "すこし",
    "ずさん",
    "すずしい",
    "すすむ",
    "すすめる",
    "すっかり",
    "ずっしり",
    "ずっと",
    "すてき",
    "すてる",
    "すねる",
    "すのこ",
    "すはだ",
    "すばらしい",
    "ずひょう",
    "ずぶぬれ",
    "すぶり",
    "すふれ",
    "すべて",
    "すべる",
    "ずほう",
    "すぼん",
    "すまい",
    "すめし",
    "すもう",
    "すやき",
    "すらすら",
    "するめ",
    "すれちがう",
    "すろっと",
    "すわる",
    "すんぜん",
    "すんぽう",
    "せあぶら",
    "せいかつ",
    "せいげん",
    "せいじ",
    "せいよう",
    "せおう",
    "せかいかん",
    "せきにん",
    "せきむ",
    "せきゆ",
    "せきらんうん",
    "せけん",
    "せこう",
    "せすじ",
    "せたい",
    "せたけ",
    "せっかく",
    "せっきゃく",
    "ぜっく",
    "せっけん",
    "せっこつ",
    "せっさたくま",
    "せつぞく",
    "せつだん",
    "せつでん",
    "せっぱん",
    "せつび",
    "せつぶん",
    "せつめい",
    "せつりつ",
    "せなか",
    "せのび",
    "せはば",
    "せびろ",
    "せぼね",
    "せまい",
    "せまる",
    "せめる",
    "せもたれ",
    "せりふ",
    "ぜんあく",
    "せんい",
    "せんえい",
    "せんか",
    "せんきょ",
    "せんく",
    "せんげん",
    "ぜんご",
    "せんさい",
    "せんしゅ",
    "せんすい",
    "せんせい",
    "せんぞ",
    "せんたく",
    "せんちょう",
    "せんてい",
    "せんとう",
    "せんぬき",
    "せんねん",
    "せんぱい",
    "ぜんぶ",
    "ぜんぽう",
    "せんむ",
    "せんめんじょ",
    "せんもん",
    "せんやく",
    "せんゆう",
    "せんよう",
    "ぜんら",
    "ぜんりゃく",
    "せんれい",
    "せんろ",
    "そあく",
    "そいとげる",
    "そいね",
    "そうがんきょう",
    "そうき",
    "そうご",
    "そうしん",
    "そうだん",
    "そうなん",
    "そうび",
    "そうめん",
    "そうり",
    "そえもの",
    "そえん",
    "そがい",
    "そげき",
    "そこう",
    "そこそこ",
    "そざい",
    "そしな",
    "そせい",
    "そせん",
    "そそぐ",
    "そだてる",
    "そつう",
    "そつえん",
    "そっかん",
    "そつぎょう",
    "そっけつ",
    "そっこう",
    "そっせん",
    "そっと",
    "そとがわ",
    "そとづら",
    "そなえる",
    "そなた",
    "そふぼ",
    "そぼく",
    "そぼろ",
    "そまつ",
    "そまる",
    "そむく",
    "そむりえ",
    "そめる",
    "そもそも",
    "そよかぜ",
    "そらまめ",
    "そろう",
    "そんかい",
    "そんけい",
    "そんざい",
    "そんしつ",
    "そんぞく",
    "そんちょう",
    "ぞんび",
    "ぞんぶん",
    "そんみん",
    "たあい",
    "たいいん",
    "たいうん",
    "たいえき",
    "たいおう",
    "だいがく",
    "たいき",
    "たいぐう",
    "たいけん",
    "たいこ",
    "たいざい",
    "だいじょうぶ",
    "だいすき",
    "たいせつ",
    "たいそう",
    "だいたい",
    "たいちょう",
    "たいてい",
    "だいどころ",
    "たいない",
    "たいねつ",
    "たいのう",
    "たいはん",
    "だいひょう",
    "たいふう",
    "たいへん",
    "たいほ",
    "たいまつばな",
    "たいみんぐ",
    "たいむ",
    "たいめん",
    "たいやき",
    "たいよう",
    "たいら",
    "たいりょく",
    "たいる",
    "たいわん",
    "たうえ",
    "たえる",
    "たおす",
    "たおる",
    "たおれる",
    "たかい",
    "たかね",
    "たきび",
    "たくさん",
    "たこく",
    "たこやき",
    "たさい",
    "たしざん",
    "だじゃれ",
    "たすける",
    "たずさわる",
    "たそがれ",
    "たたかう",
    "たたく",
    "ただしい",
    "たたみ",
    "たちばな",
    "だっかい",
    "だっきゃく",
    "だっこ",
    "だっしゅつ",
    "だったい",
    "たてる",
    "たとえる",
    "たなばた",
    "たにん",
    "たぬき",
    "たのしみ",
    "たはつ",
    "たぶん",
    "たべる",
    "たぼう",
    "たまご",
    "たまる",
    "だむる",
    "ためいき",
    "ためす",
    "ためる",
    "たもつ",
    "たやすい",
    "たよる",
    "たらす",
    "たりきほんがん",
    "たりょう",
    "たりる",
    "たると",
    "たれる",
    "たれんと",
    "たろっと",
    "たわむれる",
    "だんあつ",
    "たんい",
    "たんおん",
    "たんか",
    "たんき",
    "たんけん",
    "たんご",
    "たんさん",
    "たんじょうび",
    "だんせい",
    "たんそく",
    "たんたい",
    "だんち",
    "たんてい",
    "たんとう",
    "だんな",
    "たんにん",
    "だんねつ",
    "たんのう",
    "たんぴん",
    "だんぼう",
    "たんまつ",
    "たんめい",
    "だんれつ",
    "だんろ",
    "だんわ",
    "ちあい",
    "ちあん",
    "ちいき",
    "ちいさい",
    "ちえん",
    "ちかい",
    "ちから",
    "ちきゅう",
    "ちきん",
    "ちけいず",
    "ちけん",
    "ちこく",
    "ちさい",
    "ちしき",
    "ちしりょう",
    "ちせい",
    "ちそう",
    "ちたい",
    "ちたん",
    "ちちおや",
    "ちつじょ",
    "ちてき",
    "ちてん",
    "ちぬき",
    "ちぬり",
    "ちのう",
    "ちひょう",
    "ちへいせん",
    "ちほう",
    "ちまた",
    "ちみつ",
    "ちみどろ",
    "ちめいど",
    "ちゃんこなべ",
    "ちゅうい",
    "ちゆりょく",
    "ちょうし",
    "ちょさくけん",
    "ちらし",
    "ちらみ",
    "ちりがみ",
    "ちりょう",
    "ちるど",
    "ちわわ",
    "ちんたい",
    "ちんもく",
    "ついか",
    "ついたち",
    "つうか",
    "つうじょう",
    "つうはん",
    "つうわ",
    "つかう",
    "つかれる",
    "つくね",
    "つくる",
    "つけね",
    "つける",
    "つごう",
    "つたえる",
    "つづく",
    "つつじ",
    "つつむ",
    "つとめる",
    "つながる",
    "つなみ",
    "つねづね",
    "つのる",
    "つぶす",
    "つまらない",
    "つまる",
    "つみき",
    "つめたい",
    "つもり",
    "つもる",
    "つよい",
    "つるぼ",
    "つるみく",
    "つわもの",
    "つわり",
    "てあし",
    "てあて",
    "てあみ",
    "ていおん",
    "ていか",
    "ていき",
    "ていけい",
    "ていこく",
    "ていさつ",
    "ていし",
    "ていせい",
    "ていたい",
    "ていど",
    "ていねい",
    "ていひょう",
    "ていへん",
    "ていぼう",
    "てうち",
    "ておくれ",
    "てきとう",
    "てくび",
    "でこぼこ",
    "てさぎょう",
    "てさげ",
    "てすり",
    "てそう",
    "てちがい",
    "てちょう",
    "てつがく",
    "てつづき",
    "でっぱ",
    "てつぼう",
    "てつや",
    "でぬかえ",
    "てぬき",
    "てぬぐい",
    "てのひら",
    "てはい",
    "てぶくろ",
    "てふだ",
    "てほどき",
    "てほん",
    "てまえ",
    "てまきずし",
    "てみじか",
    "てみやげ",
    "てらす",
    "てれび",
    "てわけ",
    "てわたし",
    "でんあつ",
    "てんいん",
    "てんかい",
    "てんき",
    "てんぐ",
    "てんけん",
    "てんごく",
    "てんさい",
    "てんし",
    "てんすう",
    "でんち",
    "てんてき",
    "てんとう",
    "てんない",
    "てんぷら",
    "てんぼうだい",
    "てんめつ",
    "てんらんかい",
    "でんりょく",
    "でんわ",
    "どあい",
    "といれ",
    "どうかん",
    "とうきゅう",
    "どうぐ",
    "とうし",
    "とうむぎ",
    "とおい",
    "とおか",
    "とおく",
    "とおす",
    "とおる",
    "とかい",
    "とかす",
    "ときおり",
    "ときどき",
    "とくい",
    "とくしゅう",
    "とくてん",
    "とくに",
    "とくべつ",
    "とけい",
    "とける",
    "とこや",
    "とさか",
    "としょかん",
    "とそう",
    "とたん",
    "とちゅう",
    "とっきゅう",
    "とっくん",
    "とつぜん",
    "とつにゅう",
    "とどける",
    "ととのえる",
    "とない",
    "となえる",
    "となり",
    "とのさま",
    "とばす",
    "どぶがわ",
    "とほう",
    "とまる",
    "とめる",
    "ともだち",
    "ともる",
    "どようび",
    "とらえる",
    "とんかつ",
    "どんぶり",
    "ないかく",
    "ないこう",
    "ないしょ",
    "ないす",
    "ないせん",
    "ないそう",
    "なおす",
    "ながい",
    "なくす",
    "なげる",
    "なこうど",
    "なさけ",
    "なたでここ",
    "なっとう",
    "なつやすみ",
    "ななおし",
    "なにごと",
    "なにもの",
    "なにわ",
    "なのか",
    "なふだ",
    "なまいき",
    "なまえ",
    "なまみ",
    "なみだ",
    "なめらか",
    "なめる",
    "なやむ",
    "ならう",
    "ならび",
    "ならぶ",
    "なれる",
    "なわとび",
    "なわばり",
    "にあう",
    "にいがた",
    "にうけ",
    "におい",
    "にかい",
    "にがて",
    "にきび",
    "にくしみ",
    "にくまん",
    "にげる",
    "にさんかたんそ",
    "にしき",
    "にせもの",
    "にちじょう",
    "にちようび",
    "にっか",
    "にっき",
    "にっけい",
    "にっこう",
    "にっさん",
    "にっしょく",
    "にっすう",
    "にっせき",
    "にってい",
    "になう",
    "にほん",
    "にまめ",
    "にもつ",
    "にやり",
    "にゅういん",
    "にりんしゃ",
    "にわとり",
    "にんい",
    "にんか",
    "にんき",
    "にんげん",
    "にんしき",
    "にんずう",
    "にんそう",
    "にんたい",
    "にんち",
    "にんてい",
    "にんにく",
    "にんぷ",
    "にんまり",
    "にんむ",
    "にんめい",
    "にんよう",
    "ぬいくぎ",
    "ぬかす",
    "ぬぐいとる",
    "ぬぐう",
    "ぬくもり",
    "ぬすむ",
    "ぬまえび",
    "ぬめり",
    "ぬらす",
    "ぬんちゃく",
    "ねあげ",
    "ねいき",
    "ねいる",
    "ねいろ",
    "ねぐせ",
    "ねくたい",
    "ねくら",
    "ねこぜ",
    "ねこむ",
    "ねさげ",
    "ねすごす",
    "ねそべる",
    "ねだん",
    "ねつい",
    "ねっしん",
    "ねつぞう",
    "ねったいぎょ",
    "ねぶそく",
    "ねふだ",
    "ねぼう",
    "ねほりはほり",
    "ねまき",
    "ねまわし",
    "ねみみ",
    "ねむい",
    "ねむたい",
    "ねもと",
    "ねらう",
    "ねわざ",
    "ねんいり",
    "ねんおし",
    "ねんかん",
    "ねんきん",
    "ねんぐ",
    "ねんざ",
    "ねんし",
    "ねんちゃく",
    "ねんど",
    "ねんぴ",
    "ねんぶつ",
    "ねんまつ",
    "ねんりょう",
    "ねんれい",
    "のいず",
    "のおづま",
    "のがす",
    "のきなみ",
    "のこぎり",
    "のこす",
    "のこる",
    "のせる",
    "のぞく",
    "のぞむ",
    "のたまう",
    "のちほど",
    "のっく",
    "のばす",
    "のはら",
    "のべる",
    "のぼる",
    "のみもの",
    "のやま",
    "のらいぬ",
    "のらねこ",
    "のりもの",
    "のりゆき",
    "のれん",
    "のんき",
    "ばあい",
    "はあく",
    "ばあさん",
    "ばいか",
    "ばいく",
    "はいけん",
    "はいご",
    "はいしん",
    "はいすい",
    "はいせん",
    "はいそう",
    "はいち",
    "ばいばい",
    "はいれつ",
    "はえる",
    "はおる",
    "はかい",
    "ばかり",
    "はかる",
    "はくしゅ",
    "はけん",
    "はこぶ",
    "はさみ",
    "はさん",
    "はしご",
    "ばしょ",
    "はしる",
    "はせる",
    "ぱそこん",
    "はそん",
    "はたん",
    "はちみつ",
    "はつおん",
    "はっかく",
    "はづき",
    "はっきり",
    "はっくつ",
    "はっけん",
    "はっこう",
    "はっさん",
    "はっしん",
    "はったつ",
    "はっちゅう",
    "はってん",
    "はっぴょう",
    "はっぽう",
    "はなす",
    "はなび",
    "はにかむ",
    "はぶらし",
    "はみがき",
    "はむかう",
    "はめつ",
    "はやい",
    "はやし",
    "はらう",
    "はろうぃん",
    "はわい",
    "はんい",
    "はんえい",
    "はんおん",
    "はんかく",
    "はんきょう",
    "ばんぐみ",
    "はんこ",
    "はんしゃ",
    "はんすう",
    "はんだん",
    "ぱんち",
    "ぱんつ",
    "はんてい",
    "はんとし",
    "はんのう",
    "はんぱ",
    "はんぶん",
    "はんぺん",
    "はんぼうき",
    "はんめい",
    "はんらん",
    "はんろん",
    "ひいき",
    "ひうん",
    "ひえる",
    "ひかく",
    "ひかり",
    "ひかる",
    "ひかん",
    "ひくい",
    "ひけつ",
    "ひこうき",
    "ひこく",
    "ひさい",
    "ひさしぶり",
    "ひさん",
    "びじゅつかん",
    "ひしょ",
    "ひそか",
    "ひそむ",
    "ひたむき",
    "ひだり",
    "ひたる",
    "ひつぎ",
    "ひっこし",
    "ひっし",
    "ひつじゅひん",
    "ひっす",
    "ひつぜん",
    "ぴったり",
    "ぴっちり",
    "ひつよう",
    "ひてい",
    "ひとごみ",
    "ひなまつり",
    "ひなん",
    "ひねる",
    "ひはん",
    "ひびく",
    "ひひょう",
    "ひほう",
    "ひまわり",
    "ひまん",
    "ひみつ",
    "ひめい",
    "ひめじし",
    "ひやけ",
    "ひやす",
    "ひよう",
    "びょうき",
    "ひらがな",
    "ひらく",
    "ひりつ",
    "ひりょう",
    "ひるま",
    "ひるやすみ",
    "ひれい",
    "ひろい",
    "ひろう",
    "ひろき",
    "ひろゆき",
    "ひんかく",
    "ひんけつ",
    "ひんこん",
    "ひんしゅ",
    "ひんそう",
    "ぴんち",
    "ひんぱん",
    "びんぼう",
    "ふあん",
    "ふいうち",
    "ふうけい",
    "ふうせん",
    "ぷうたろう",
    "ふうとう",
    "ふうふ",
    "ふえる",
    "ふおん",
    "ふかい",
    "ふきん",
    "ふくざつ",
    "ふくぶくろ",
    "ふこう",
    "ふさい",
    "ふしぎ",
    "ふじみ",
    "ふすま",
    "ふせい",
    "ふせぐ",
    "ふそく",
    "ぶたにく",
    "ふたん",
    "ふちょう",
    "ふつう",
    "ふつか",
    "ふっかつ",
    "ふっき",
    "ふっこく",
    "ぶどう",
    "ふとる",
    "ふとん",
    "ふのう",
    "ふはい",
    "ふひょう",
    "ふへん",
    "ふまん",
    "ふみん",
    "ふめつ",
    "ふめん",
    "ふよう",
    "ふりこ",
    "ふりる",
    "ふるい",
    "ふんいき",
    "ぶんがく",
    "ぶんぐ",
    "ふんしつ",
    "ぶんせき",
    "ふんそう",
    "ぶんぽう",
    "へいあん",
    "へいおん",
    "へいがい",
    "へいき",
    "へいげん",
    "へいこう",
    "へいさ",
    "へいしゃ",
    "へいせつ",
    "へいそ",
    "へいたく",
    "へいてん",
    "へいねつ",
    "へいわ",
    "へきが",
    "へこむ",
    "べにいろ",
    "べにしょうが",
    "へらす",
    "へんかん",
    "べんきょう",
    "べんごし",
    "へんさい",
    "へんたい",
    "べんり",
    "ほあん",
    "ほいく",
    "ぼうぎょ",
    "ほうこく",
    "ほうそう",
    "ほうほう",
    "ほうもん",
    "ほうりつ",
    "ほえる",
    "ほおん",
    "ほかん",
    "ほきょう",
    "ぼきん",
    "ほくろ",
    "ほけつ",
    "ほけん",
    "ほこう",
    "ほこる",
    "ほしい",
    "ほしつ",
    "ほしゅ",
    "ほしょう",
    "ほせい",
    "ほそい",
    "ほそく",
    "ほたて",
    "ほたる",
    "ぽちぶくろ",
    "ほっきょく",
    "ほっさ",
    "ほったん",
    "ほとんど",
    "ほめる",
    "ほんい",
    "ほんき",
    "ほんけ",
    "ほんしつ",
    "ほんやく",
    "まいにち",
    "まかい",
    "まかせる",
    "まがる",
    "まける",
    "まこと",
    "まさつ",
    "まじめ",
    "ますく",
    "まぜる",
    "まつり",
    "まとめ",
    "まなぶ",
    "まぬけ",
    "まねく",
    "まほう",
    "まもる",
    "まゆげ",
    "まよう",
    "まろやか",
    "まわす",
    "まわり",
    "まわる",
    "まんが",
    "まんきつ",
    "まんぞく",
    "まんなか",
    "みいら",
    "みうち",
    "みえる",
    "みがく",
    "みかた",
    "みかん",
    "みけん",
    "みこん",
    "みじかい",
    "みすい",
    "みすえる",
    "みせる",
    "みっか",
    "みつかる",
    "みつける",
    "みてい",
    "みとめる",
    "みなと",
    "みなみかさい",
    "みねらる",
    "みのう",
    "みのがす",
    "みほん",
    "みもと",
    "みやげ",
    "みらい",
    "みりょく",
    "みわく",
    "みんか",
    "みんぞく",
    "むいか",
    "むえき",
    "むえん",
    "むかい",
    "むかう",
    "むかえ",
    "むかし",
    "むぎちゃ",
    "むける",
    "むげん",
    "むさぼる",
    "むしあつい",
    "むしば",
    "むじゅん",
    "むしろ",
    "むすう",
    "むすこ",
    "むすぶ",
    "むすめ",
    "むせる",
    "むせん",
    "むちゅう",
    "むなしい",
    "むのう",
    "むやみ",
    "むよう",
    "むらさき",
    "むりょう",
    "むろん",
    "めいあん",
    "めいうん",
    "めいえん",
    "めいかく",
    "めいきょく",
    "めいさい",
    "めいし",
    "めいそう",
    "めいぶつ",
    "めいれい",
    "めいわく",
    "めぐまれる",
    "めざす",
    "めした",
    "めずらしい",
    "めだつ",
    "めまい",
    "めやす",
    "めんきょ",
    "めんせき",
    "めんどう",
    "もうしあげる",
    "もうどうけん",
    "もえる",
    "もくし",
    "もくてき",
    "もくようび",
    "もちろん",
    "もどる",
    "もらう",
    "もんく",
    "もんだい",
    "やおや",
    "やける",
    "やさい",
    "やさしい",
    "やすい",
    "やすたろう",
    "やすみ",
    "やせる",
    "やそう",
    "やたい",
    "やちん",
    "やっと",
    "やっぱり",
    "やぶる",
    "やめる",
    "ややこしい",
    "やよい",
    "やわらかい",
    "ゆうき",
    "ゆうびんきょく",
    "ゆうべ",
    "ゆうめい",
    "ゆけつ",
    "ゆしゅつ",
    "ゆせん",
    "ゆそう",
    "ゆたか",
    "ゆちゃく",
    "ゆでる",
    "ゆにゅう",
    "ゆびわ",
    "ゆらい",
    "ゆれる",
    "ようい",
    "ようか",
    "ようきゅう",
    "ようじ",
    "ようす",
    "ようちえん",
    "よかぜ",
    "よかん",
    "よきん",
    "よくせい",
    "よくぼう",
    "よけい",
    "よごれる",
    "よさん",
    "よしゅう",
    "よそう",
    "よそく",
    "よっか",
    "よてい",
    "よどがわく",
    "よねつ",
    "よやく",
    "よゆう",
    "よろこぶ",
    "よろしい",
    "らいう",
    "らくがき",
    "らくご",
    "らくさつ",
    "らくだ",
    "らしんばん",
    "らせん",
    "らぞく",
    "らたい",
    "らっか",
    "られつ",
    "りえき",
    "りかい",
    "りきさく",
    "りきせつ",
    "りくぐん",
    "りくつ",
    "りけん",
    "りこう",
    "りせい",
    "りそう",
    "りそく",
    "りてん",
    "りねん",
    "りゆう",
    "りゅうがく",
    "りよう",
    "りょうり",
    "りょかん",
    "りょくちゃ",
    "りょこう",
    "りりく",
    "りれき",
    "りろん",
    "りんご",
    "るいけい",
    "るいさい",
    "るいじ",
    "るいせき",
    "るすばん",
    "るりがわら",
    "れいかん",
    "れいぎ",
    "れいせい",
    "れいぞうこ",
    "れいとう",
    "れいぼう",
    "れきし",
    "れきだい",
    "れんあい",
    "れんけい",
    "れんこん",
    "れんさい",
    "れんしゅう",
    "れんぞく",
    "れんらく",
    "ろうか",
    "ろうご",
    "ろうじん",
    "ろうそく",
    "ろくが",
    "ろこつ",
    "ろじうら",
    "ろしゅつ",
    "ろせん",
    "ろてん",
    "ろめん",
    "ろれつ",
    "ろんぎ",
    "ろんぱ",
    "ろんぶん",
    "ろんり",
    "わかす",
    "わかめ",
    "わかやま",
    "わかれる",
    "わしつ",
    "わじまし",
    "わすれもの",
    "わらう",
    "われる"
]

},{}],15:[function(require,module,exports){
module.exports=[
    "가격",
    "가끔",
    "가난",
    "가능",
    "가득",
    "가르침",
    "가뭄",
    "가방",
    "가상",
    "가슴",
    "가운데",
    "가을",
    "가이드",
    "가입",
    "가장",
    "가정",
    "가족",
    "가죽",
    "각오",
    "각자",
    "간격",
    "간부",
    "간섭",
    "간장",
    "간접",
    "간판",
    "갈등",
    "갈비",
    "갈색",
    "갈증",
    "감각",
    "감기",
    "감소",
    "감수성",
    "감자",
    "감정",
    "갑자기",
    "강남",
    "강당",
    "강도",
    "강력히",
    "강변",
    "강북",
    "강사",
    "강수량",
    "강아지",
    "강원도",
    "강의",
    "강제",
    "강조",
    "같이",
    "개구리",
    "개나리",
    "개방",
    "개별",
    "개선",
    "개성",
    "개인",
    "객관적",
    "거실",
    "거액",
    "거울",
    "거짓",
    "거품",
    "걱정",
    "건강",
    "건물",
    "건설",
    "건조",
    "건축",
    "걸음",
    "검사",
    "검토",
    "게시판",
    "게임",
    "겨울",
    "견해",
    "결과",
    "결국",
    "결론",
    "결석",
    "결승",
    "결심",
    "결정",
    "결혼",
    "경계",
    "경고",
    "경기",
    "경력",
    "경복궁",
    "경비",
    "경상도",
    "경영",
    "경우",
    "경쟁",
    "경제",
    "경주",
    "경찰",
    "경치",
    "경향",
    "경험",
    "계곡",
    "계단",
    "계란",
    "계산",
    "계속",
    "계약",
    "계절",
    "계층",
    "계획",
    "고객",
    "고구려",
    "고궁",
    "고급",
    "고등학생",
    "고무신",
    "고민",
    "고양이",
    "고장",
    "고전",
    "고집",
    "고춧가루",
    "고통",
    "고향",
    "곡식",
    "골목",
    "골짜기",
    "골프",
    "공간",
    "공개",
    "공격",
    "공군",
    "공급",
    "공기",
    "공동",
    "공무원",
    "공부",
    "공사",
    "공식",
    "공업",
    "공연",
    "공원",
    "공장",
    "공짜",
    "공책",
    "공통",
    "공포",
    "공항",
    "공휴일",
    "과목",
    "과일",
    "과장",
    "과정",
    "과학",
    "관객",
    "관계",
    "관광",
    "관념",
    "관람",
    "관련",
    "관리",
    "관습",
    "관심",
    "관점",
    "관찰",
    "광경",
    "광고",
    "광장",
    "광주",
    "괴로움",
    "굉장히",
    "교과서",
    "교문",
    "교복",
    "교실",
    "교양",
    "교육",
    "교장",
    "교직",
    "교통",
    "교환",
    "교훈",
    "구경",
    "구름",
    "구멍",
    "구별",
    "구분",
    "구석",
    "구성",
    "구속",
    "구역",
    "구입",
    "구청",
    "구체적",
    "국가",
    "국기",
    "국내",
    "국립",
    "국물",
    "국민",
    "국수",
    "국어",
    "국왕",
    "국적",
    "국제",
    "국회",
    "군대",
    "군사",
    "군인",
    "궁극적",
    "권리",
    "권위",
    "권투",
    "귀국",
    "귀신",
    "규정",
    "규칙",
    "균형",
    "그날",
    "그냥",
    "그늘",
    "그러나",
    "그룹",
    "그릇",
    "그림",
    "그제서야",
    "그토록",
    "극복",
    "극히",
    "근거",
    "근교",
    "근래",
    "근로",
    "근무",
    "근본",
    "근원",
    "근육",
    "근처",
    "글씨",
    "글자",
    "금강산",
    "금고",
    "금년",
    "금메달",
    "금액",
    "금연",
    "금요일",
    "금지",
    "긍정적",
    "기간",
    "기관",
    "기념",
    "기능",
    "기독교",
    "기둥",
    "기록",
    "기름",
    "기법",
    "기본",
    "기분",
    "기쁨",
    "기숙사",
    "기술",
    "기억",
    "기업",
    "기온",
    "기운",
    "기원",
    "기적",
    "기준",
    "기침",
    "기혼",
    "기획",
    "긴급",
    "긴장",
    "길이",
    "김밥",
    "김치",
    "김포공항",
    "깍두기",
    "깜빡",
    "깨달음",
    "깨소금",
    "껍질",
    "꼭대기",
    "꽃잎",
    "나들이",
    "나란히",
    "나머지",
    "나물",
    "나침반",
    "나흘",
    "낙엽",
    "난방",
    "날개",
    "날씨",
    "날짜",
    "남녀",
    "남대문",
    "남매",
    "남산",
    "남자",
    "남편",
    "남학생",
    "낭비",
    "낱말",
    "내년",
    "내용",
    "내일",
    "냄비",
    "냄새",
    "냇물",
    "냉동",
    "냉면",
    "냉방",
    "냉장고",
    "넥타이",
    "넷째",
    "노동",
    "노란색",
    "노력",
    "노인",
    "녹음",
    "녹차",
    "녹화",
    "논리",
    "논문",
    "논쟁",
    "놀이",
    "농구",
    "농담",
    "농민",
    "농부",
    "농업",
    "농장",
    "농촌",
    "높이",
    "눈동자",
    "눈물",
    "눈썹",
    "뉴욕",
    "느낌",
    "늑대",
    "능동적",
    "능력",
    "다방",
    "다양성",
    "다음",
    "다이어트",
    "다행",
    "단계",
    "단골",
    "단독",
    "단맛",
    "단순",
    "단어",
    "단위",
    "단점",
    "단체",
    "단추",
    "단편",
    "단풍",
    "달걀",
    "달러",
    "달력",
    "달리",
    "닭고기",
    "담당",
    "담배",
    "담요",
    "담임",
    "답변",
    "답장",
    "당근",
    "당분간",
    "당연히",
    "당장",
    "대규모",
    "대낮",
    "대단히",
    "대답",
    "대도시",
    "대략",
    "대량",
    "대륙",
    "대문",
    "대부분",
    "대신",
    "대응",
    "대장",
    "대전",
    "대접",
    "대중",
    "대책",
    "대출",
    "대충",
    "대통령",
    "대학",
    "대한민국",
    "대합실",
    "대형",
    "덩어리",
    "데이트",
    "도대체",
    "도덕",
    "도둑",
    "도망",
    "도서관",
    "도심",
    "도움",
    "도입",
    "도자기",
    "도저히",
    "도전",
    "도중",
    "도착",
    "독감",
    "독립",
    "독서",
    "독일",
    "독창적",
    "동화책",
    "뒷모습",
    "뒷산",
    "딸아이",
    "마누라",
    "마늘",
    "마당",
    "마라톤",
    "마련",
    "마무리",
    "마사지",
    "마약",
    "마요네즈",
    "마을",
    "마음",
    "마이크",
    "마중",
    "마지막",
    "마찬가지",
    "마찰",
    "마흔",
    "막걸리",
    "막내",
    "막상",
    "만남",
    "만두",
    "만세",
    "만약",
    "만일",
    "만점",
    "만족",
    "만화",
    "많이",
    "말기",
    "말씀",
    "말투",
    "맘대로",
    "망원경",
    "매년",
    "매달",
    "매력",
    "매번",
    "매스컴",
    "매일",
    "매장",
    "맥주",
    "먹이",
    "먼저",
    "먼지",
    "멀리",
    "메일",
    "며느리",
    "며칠",
    "면담",
    "멸치",
    "명단",
    "명령",
    "명예",
    "명의",
    "명절",
    "명칭",
    "명함",
    "모금",
    "모니터",
    "모델",
    "모든",
    "모범",
    "모습",
    "모양",
    "모임",
    "모조리",
    "모집",
    "모퉁이",
    "목걸이",
    "목록",
    "목사",
    "목소리",
    "목숨",
    "목적",
    "목표",
    "몰래",
    "몸매",
    "몸무게",
    "몸살",
    "몸속",
    "몸짓",
    "몸통",
    "몹시",
    "무관심",
    "무궁화",
    "무더위",
    "무덤",
    "무릎",
    "무슨",
    "무엇",
    "무역",
    "무용",
    "무조건",
    "무지개",
    "무척",
    "문구",
    "문득",
    "문법",
    "문서",
    "문제",
    "문학",
    "문화",
    "물가",
    "물건",
    "물결",
    "물고기",
    "물론",
    "물리학",
    "물음",
    "물질",
    "물체",
    "미국",
    "미디어",
    "미사일",
    "미술",
    "미역",
    "미용실",
    "미움",
    "미인",
    "미팅",
    "미혼",
    "민간",
    "민족",
    "민주",
    "믿음",
    "밀가루",
    "밀리미터",
    "밑바닥",
    "바가지",
    "바구니",
    "바나나",
    "바늘",
    "바닥",
    "바닷가",
    "바람",
    "바이러스",
    "바탕",
    "박물관",
    "박사",
    "박수",
    "반대",
    "반드시",
    "반말",
    "반발",
    "반성",
    "반응",
    "반장",
    "반죽",
    "반지",
    "반찬",
    "받침",
    "발가락",
    "발걸음",
    "발견",
    "발달",
    "발레",
    "발목",
    "발바닥",
    "발생",
    "발음",
    "발자국",
    "발전",
    "발톱",
    "발표",
    "밤하늘",
    "밥그릇",
    "밥맛",
    "밥상",
    "밥솥",
    "방금",
    "방면",
    "방문",
    "방바닥",
    "방법",
    "방송",
    "방식",
    "방안",
    "방울",
    "방지",
    "방학",
    "방해",
    "방향",
    "배경",
    "배꼽",
    "배달",
    "배드민턴",
    "백두산",
    "백색",
    "백성",
    "백인",
    "백제",
    "백화점",
    "버릇",
    "버섯",
    "버튼",
    "번개",
    "번역",
    "번지",
    "번호",
    "벌금",
    "벌레",
    "벌써",
    "범위",
    "범인",
    "범죄",
    "법률",
    "법원",
    "법적",
    "법칙",
    "베이징",
    "벨트",
    "변경",
    "변동",
    "변명",
    "변신",
    "변호사",
    "변화",
    "별도",
    "별명",
    "별일",
    "병실",
    "병아리",
    "병원",
    "보관",
    "보너스",
    "보라색",
    "보람",
    "보름",
    "보상",
    "보안",
    "보자기",
    "보장",
    "보전",
    "보존",
    "보통",
    "보편적",
    "보험",
    "복도",
    "복사",
    "복숭아",
    "복습",
    "볶음",
    "본격적",
    "본래",
    "본부",
    "본사",
    "본성",
    "본인",
    "본질",
    "볼펜",
    "봉사",
    "봉지",
    "봉투",
    "부근",
    "부끄러움",
    "부담",
    "부동산",
    "부문",
    "부분",
    "부산",
    "부상",
    "부엌",
    "부인",
    "부작용",
    "부장",
    "부정",
    "부족",
    "부지런히",
    "부친",
    "부탁",
    "부품",
    "부회장",
    "북부",
    "북한",
    "분노",
    "분량",
    "분리",
    "분명",
    "분석",
    "분야",
    "분위기",
    "분필",
    "분홍색",
    "불고기",
    "불과",
    "불교",
    "불꽃",
    "불만",
    "불법",
    "불빛",
    "불안",
    "불이익",
    "불행",
    "브랜드",
    "비극",
    "비난",
    "비닐",
    "비둘기",
    "비디오",
    "비로소",
    "비만",
    "비명",
    "비밀",
    "비바람",
    "비빔밥",
    "비상",
    "비용",
    "비율",
    "비중",
    "비타민",
    "비판",
    "빌딩",
    "빗물",
    "빗방울",
    "빗줄기",
    "빛깔",
    "빨간색",
    "빨래",
    "빨리",
    "사건",
    "사계절",
    "사나이",
    "사냥",
    "사람",
    "사랑",
    "사립",
    "사모님",
    "사물",
    "사방",
    "사상",
    "사생활",
    "사설",
    "사슴",
    "사실",
    "사업",
    "사용",
    "사월",
    "사장",
    "사전",
    "사진",
    "사촌",
    "사춘기",
    "사탕",
    "사투리",
    "사흘",
    "산길",
    "산부인과",
    "산업",
    "산책",
    "살림",
    "살인",
    "살짝",
    "삼계탕",
    "삼국",
    "삼십",
    "삼월",
    "삼촌",
    "상관",
    "상금",
    "상대",
    "상류",
    "상반기",
    "상상",
    "상식",
    "상업",
    "상인",
    "상자",
    "상점",
    "상처",
    "상추",
    "상태",
    "상표",
    "상품",
    "상황",
    "새벽",
    "색깔",
    "색연필",
    "생각",
    "생명",
    "생물",
    "생방송",
    "생산",
    "생선",
    "생신",
    "생일",
    "생활",
    "서랍",
    "서른",
    "서명",
    "서민",
    "서비스",
    "서양",
    "서울",
    "서적",
    "서점",
    "서쪽",
    "서클",
    "석사",
    "석유",
    "선거",
    "선물",
    "선배",
    "선생",
    "선수",
    "선원",
    "선장",
    "선전",
    "선택",
    "선풍기",
    "설거지",
    "설날",
    "설렁탕",
    "설명",
    "설문",
    "설사",
    "설악산",
    "설치",
    "설탕",
    "섭씨",
    "성공",
    "성당",
    "성명",
    "성별",
    "성인",
    "성장",
    "성적",
    "성질",
    "성함",
    "세금",
    "세미나",
    "세상",
    "세월",
    "세종대왕",
    "세탁",
    "센터",
    "센티미터",
    "셋째",
    "소규모",
    "소극적",
    "소금",
    "소나기",
    "소년",
    "소득",
    "소망",
    "소문",
    "소설",
    "소속",
    "소아과",
    "소용",
    "소원",
    "소음",
    "소중히",
    "소지품",
    "소질",
    "소풍",
    "소형",
    "속담",
    "속도",
    "속옷",
    "손가락",
    "손길",
    "손녀",
    "손님",
    "손등",
    "손목",
    "손뼉",
    "손실",
    "손질",
    "손톱",
    "손해",
    "솔직히",
    "솜씨",
    "송아지",
    "송이",
    "송편",
    "쇠고기",
    "쇼핑",
    "수건",
    "수년",
    "수단",
    "수돗물",
    "수동적",
    "수면",
    "수명",
    "수박",
    "수상",
    "수석",
    "수술",
    "수시로",
    "수업",
    "수염",
    "수영",
    "수입",
    "수준",
    "수집",
    "수출",
    "수컷",
    "수필",
    "수학",
    "수험생",
    "수화기",
    "숙녀",
    "숙소",
    "숙제",
    "순간",
    "순서",
    "순수",
    "순식간",
    "순위",
    "숟가락",
    "술병",
    "술집",
    "숫자",
    "스님",
    "스물",
    "스스로",
    "스승",
    "스웨터",
    "스위치",
    "스케이트",
    "스튜디오",
    "스트레스",
    "스포츠",
    "슬쩍",
    "슬픔",
    "습관",
    "습기",
    "승객",
    "승리",
    "승부",
    "승용차",
    "승진",
    "시각",
    "시간",
    "시골",
    "시금치",
    "시나리오",
    "시댁",
    "시리즈",
    "시멘트",
    "시민",
    "시부모",
    "시선",
    "시설",
    "시스템",
    "시아버지",
    "시어머니",
    "시월",
    "시인",
    "시일",
    "시작",
    "시장",
    "시절",
    "시점",
    "시중",
    "시즌",
    "시집",
    "시청",
    "시합",
    "시험",
    "식구",
    "식기",
    "식당",
    "식량",
    "식료품",
    "식물",
    "식빵",
    "식사",
    "식생활",
    "식초",
    "식탁",
    "식품",
    "신고",
    "신규",
    "신념",
    "신문",
    "신발",
    "신비",
    "신사",
    "신세",
    "신용",
    "신제품",
    "신청",
    "신체",
    "신화",
    "실감",
    "실내",
    "실력",
    "실례",
    "실망",
    "실수",
    "실습",
    "실시",
    "실장",
    "실정",
    "실질적",
    "실천",
    "실체",
    "실컷",
    "실태",
    "실패",
    "실험",
    "실현",
    "심리",
    "심부름",
    "심사",
    "심장",
    "심정",
    "심판",
    "쌍둥이",
    "씨름",
    "씨앗",
    "아가씨",
    "아나운서",
    "아드님",
    "아들",
    "아쉬움",
    "아스팔트",
    "아시아",
    "아울러",
    "아저씨",
    "아줌마",
    "아직",
    "아침",
    "아파트",
    "아프리카",
    "아픔",
    "아홉",
    "아흔",
    "악기",
    "악몽",
    "악수",
    "안개",
    "안경",
    "안과",
    "안내",
    "안녕",
    "안동",
    "안방",
    "안부",
    "안주",
    "알루미늄",
    "알코올",
    "암시",
    "암컷",
    "압력",
    "앞날",
    "앞문",
    "애인",
    "애정",
    "액수",
    "앨범",
    "야간",
    "야단",
    "야옹",
    "약간",
    "약국",
    "약속",
    "약수",
    "약점",
    "약품",
    "약혼녀",
    "양념",
    "양력",
    "양말",
    "양배추",
    "양주",
    "양파",
    "어둠",
    "어려움",
    "어른",
    "어젯밤",
    "어쨌든",
    "어쩌다가",
    "어쩐지",
    "언니",
    "언덕",
    "언론",
    "언어",
    "얼굴",
    "얼른",
    "얼음",
    "얼핏",
    "엄마",
    "업무",
    "업종",
    "업체",
    "엉덩이",
    "엉망",
    "엉터리",
    "엊그제",
    "에너지",
    "에어컨",
    "엔진",
    "여건",
    "여고생",
    "여관",
    "여군",
    "여권",
    "여대생",
    "여덟",
    "여동생",
    "여든",
    "여론",
    "여름",
    "여섯",
    "여성",
    "여왕",
    "여인",
    "여전히",
    "여직원",
    "여학생",
    "여행",
    "역사",
    "역시",
    "역할",
    "연결",
    "연구",
    "연극",
    "연기",
    "연락",
    "연설",
    "연세",
    "연속",
    "연습",
    "연애",
    "연예인",
    "연인",
    "연장",
    "연주",
    "연출",
    "연필",
    "연합",
    "연휴",
    "열기",
    "열매",
    "열쇠",
    "열심히",
    "열정",
    "열차",
    "열흘",
    "염려",
    "엽서",
    "영국",
    "영남",
    "영상",
    "영양",
    "영역",
    "영웅",
    "영원히",
    "영하",
    "영향",
    "영혼",
    "영화",
    "옆구리",
    "옆방",
    "옆집",
    "예감",
    "예금",
    "예방",
    "예산",
    "예상",
    "예선",
    "예술",
    "예습",
    "예식장",
    "예약",
    "예전",
    "예절",
    "예정",
    "예컨대",
    "옛날",
    "오늘",
    "오락",
    "오랫동안",
    "오렌지",
    "오로지",
    "오른발",
    "오븐",
    "오십",
    "오염",
    "오월",
    "오전",
    "오직",
    "오징어",
    "오페라",
    "오피스텔",
    "오히려",
    "옥상",
    "옥수수",
    "온갖",
    "온라인",
    "온몸",
    "온종일",
    "온통",
    "올가을",
    "올림픽",
    "올해",
    "옷차림",
    "와이셔츠",
    "와인",
    "완성",
    "완전",
    "왕비",
    "왕자",
    "왜냐하면",
    "왠지",
    "외갓집",
    "외국",
    "외로움",
    "외삼촌",
    "외출",
    "외침",
    "외할머니",
    "왼발",
    "왼손",
    "왼쪽",
    "요금",
    "요일",
    "요즘",
    "요청",
    "용기",
    "용서",
    "용어",
    "우산",
    "우선",
    "우승",
    "우연히",
    "우정",
    "우체국",
    "우편",
    "운동",
    "운명",
    "운반",
    "운전",
    "운행",
    "울산",
    "울음",
    "움직임",
    "웃어른",
    "웃음",
    "워낙",
    "원고",
    "원래",
    "원서",
    "원숭이",
    "원인",
    "원장",
    "원피스",
    "월급",
    "월드컵",
    "월세",
    "월요일",
    "웨이터",
    "위반",
    "위법",
    "위성",
    "위원",
    "위험",
    "위협",
    "윗사람",
    "유난히",
    "유럽",
    "유명",
    "유물",
    "유산",
    "유적",
    "유치원",
    "유학",
    "유행",
    "유형",
    "육군",
    "육상",
    "육십",
    "육체",
    "은행",
    "음력",
    "음료",
    "음반",
    "음성",
    "음식",
    "음악",
    "음주",
    "의견",
    "의논",
    "의문",
    "의복",
    "의식",
    "의심",
    "의외로",
    "의욕",
    "의원",
    "의학",
    "이것",
    "이곳",
    "이념",
    "이놈",
    "이달",
    "이대로",
    "이동",
    "이렇게",
    "이력서",
    "이론적",
    "이름",
    "이민",
    "이발소",
    "이별",
    "이불",
    "이빨",
    "이상",
    "이성",
    "이슬",
    "이야기",
    "이용",
    "이웃",
    "이월",
    "이윽고",
    "이익",
    "이전",
    "이중",
    "이튿날",
    "이틀",
    "이혼",
    "인간",
    "인격",
    "인공",
    "인구",
    "인근",
    "인기",
    "인도",
    "인류",
    "인물",
    "인생",
    "인쇄",
    "인연",
    "인원",
    "인재",
    "인종",
    "인천",
    "인체",
    "인터넷",
    "인하",
    "인형",
    "일곱",
    "일기",
    "일단",
    "일대",
    "일등",
    "일반",
    "일본",
    "일부",
    "일상",
    "일생",
    "일손",
    "일요일",
    "일월",
    "일정",
    "일종",
    "일주일",
    "일찍",
    "일체",
    "일치",
    "일행",
    "일회용",
    "임금",
    "임무",
    "입대",
    "입력",
    "입맛",
    "입사",
    "입술",
    "입시",
    "입원",
    "입장",
    "입학",
    "자가용",
    "자격",
    "자극",
    "자동",
    "자랑",
    "자부심",
    "자식",
    "자신",
    "자연",
    "자원",
    "자율",
    "자전거",
    "자정",
    "자존심",
    "자판",
    "작가",
    "작년",
    "작성",
    "작업",
    "작용",
    "작은딸",
    "작품",
    "잔디",
    "잔뜩",
    "잔치",
    "잘못",
    "잠깐",
    "잠수함",
    "잠시",
    "잠옷",
    "잠자리",
    "잡지",
    "장관",
    "장군",
    "장기간",
    "장래",
    "장례",
    "장르",
    "장마",
    "장면",
    "장모",
    "장미",
    "장비",
    "장사",
    "장소",
    "장식",
    "장애인",
    "장인",
    "장점",
    "장차",
    "장학금",
    "재능",
    "재빨리",
    "재산",
    "재생",
    "재작년",
    "재정",
    "재채기",
    "재판",
    "재학",
    "재활용",
    "저것",
    "저고리",
    "저곳",
    "저녁",
    "저런",
    "저렇게",
    "저번",
    "저울",
    "저절로",
    "저축",
    "적극",
    "적당히",
    "적성",
    "적용",
    "적응",
    "전개",
    "전공",
    "전기",
    "전달",
    "전라도",
    "전망",
    "전문",
    "전반",
    "전부",
    "전세",
    "전시",
    "전용",
    "전자",
    "전쟁",
    "전주",
    "전철",
    "전체",
    "전통",
    "전혀",
    "전후",
    "절대",
    "절망",
    "절반",
    "절약",
    "절차",
    "점검",
    "점수",
    "점심",
    "점원",
    "점점",
    "점차",
    "접근",
    "접시",
    "접촉",
    "젓가락",
    "정거장",
    "정도",
    "정류장",
    "정리",
    "정말",
    "정면",
    "정문",
    "정반대",
    "정보",
    "정부",
    "정비",
    "정상",
    "정성",
    "정오",
    "정원",
    "정장",
    "정지",
    "정치",
    "정확히",
    "제공",
    "제과점",
    "제대로",
    "제목",
    "제발",
    "제법",
    "제삿날",
    "제안",
    "제일",
    "제작",
    "제주도",
    "제출",
    "제품",
    "제한",
    "조각",
    "조건",
    "조금",
    "조깅",
    "조명",
    "조미료",
    "조상",
    "조선",
    "조용히",
    "조절",
    "조정",
    "조직",
    "존댓말",
    "존재",
    "졸업",
    "졸음",
    "종교",
    "종로",
    "종류",
    "종소리",
    "종업원",
    "종종",
    "종합",
    "좌석",
    "죄인",
    "주관적",
    "주름",
    "주말",
    "주머니",
    "주먹",
    "주문",
    "주민",
    "주방",
    "주변",
    "주식",
    "주인",
    "주일",
    "주장",
    "주전자",
    "주택",
    "준비",
    "줄거리",
    "줄기",
    "줄무늬",
    "중간",
    "중계방송",
    "중국",
    "중년",
    "중단",
    "중독",
    "중반",
    "중부",
    "중세",
    "중소기업",
    "중순",
    "중앙",
    "중요",
    "중학교",
    "즉석",
    "즉시",
    "즐거움",
    "증가",
    "증거",
    "증권",
    "증상",
    "증세",
    "지각",
    "지갑",
    "지경",
    "지극히",
    "지금",
    "지급",
    "지능",
    "지름길",
    "지리산",
    "지방",
    "지붕",
    "지식",
    "지역",
    "지우개",
    "지원",
    "지적",
    "지점",
    "지진",
    "지출",
    "직선",
    "직업",
    "직원",
    "직장",
    "진급",
    "진동",
    "진로",
    "진료",
    "진리",
    "진짜",
    "진찰",
    "진출",
    "진통",
    "진행",
    "질문",
    "질병",
    "질서",
    "짐작",
    "집단",
    "집안",
    "집중",
    "짜증",
    "찌꺼기",
    "차남",
    "차라리",
    "차량",
    "차림",
    "차별",
    "차선",
    "차츰",
    "착각",
    "찬물",
    "찬성",
    "참가",
    "참기름",
    "참새",
    "참석",
    "참여",
    "참외",
    "참조",
    "찻잔",
    "창가",
    "창고",
    "창구",
    "창문",
    "창밖",
    "창작",
    "창조",
    "채널",
    "채점",
    "책가방",
    "책방",
    "책상",
    "책임",
    "챔피언",
    "처벌",
    "처음",
    "천국",
    "천둥",
    "천장",
    "천재",
    "천천히",
    "철도",
    "철저히",
    "철학",
    "첫날",
    "첫째",
    "청년",
    "청바지",
    "청소",
    "청춘",
    "체계",
    "체력",
    "체온",
    "체육",
    "체중",
    "체험",
    "초등학생",
    "초반",
    "초밥",
    "초상화",
    "초순",
    "초여름",
    "초원",
    "초저녁",
    "초점",
    "초청",
    "초콜릿",
    "촛불",
    "총각",
    "총리",
    "총장",
    "촬영",
    "최근",
    "최상",
    "최선",
    "최신",
    "최악",
    "최종",
    "추석",
    "추억",
    "추진",
    "추천",
    "추측",
    "축구",
    "축소",
    "축제",
    "축하",
    "출근",
    "출발",
    "출산",
    "출신",
    "출연",
    "출입",
    "출장",
    "출판",
    "충격",
    "충고",
    "충돌",
    "충분히",
    "충청도",
    "취업",
    "취직",
    "취향",
    "치약",
    "친구",
    "친척",
    "칠십",
    "칠월",
    "칠판",
    "침대",
    "침묵",
    "침실",
    "칫솔",
    "칭찬",
    "카메라",
    "카운터",
    "칼국수",
    "캐릭터",
    "캠퍼스",
    "캠페인",
    "커튼",
    "컨디션",
    "컬러",
    "컴퓨터",
    "코끼리",
    "코미디",
    "콘서트",
    "콜라",
    "콤플렉스",
    "콩나물",
    "쾌감",
    "쿠데타",
    "크림",
    "큰길",
    "큰딸",
    "큰소리",
    "큰아들",
    "큰어머니",
    "큰일",
    "큰절",
    "클래식",
    "클럽",
    "킬로",
    "타입",
    "타자기",
    "탁구",
    "탁자",
    "탄생",
    "태권도",
    "태양",
    "태풍",
    "택시",
    "탤런트",
    "터널",
    "터미널",
    "테니스",
    "테스트",
    "테이블",
    "텔레비전",
    "토론",
    "토마토",
    "토요일",
    "통계",
    "통과",
    "통로",
    "통신",
    "통역",
    "통일",
    "통장",
    "통제",
    "통증",
    "통합",
    "통화",
    "퇴근",
    "퇴원",
    "퇴직금",
    "튀김",
    "트럭",
    "특급",
    "특별",
    "특성",
    "특수",
    "특징",
    "특히",
    "튼튼히",
    "티셔츠",
    "파란색",
    "파일",
    "파출소",
    "판결",
    "판단",
    "판매",
    "판사",
    "팔십",
    "팔월",
    "팝송",
    "패션",
    "팩스",
    "팩시밀리",
    "팬티",
    "퍼센트",
    "페인트",
    "편견",
    "편의",
    "편지",
    "편히",
    "평가",
    "평균",
    "평생",
    "평소",
    "평양",
    "평일",
    "평화",
    "포스터",
    "포인트",
    "포장",
    "포함",
    "표면",
    "표정",
    "표준",
    "표현",
    "품목",
    "품질",
    "풍경",
    "풍속",
    "풍습",
    "프랑스",
    "프린터",
    "플라스틱",
    "피곤",
    "피망",
    "피아노",
    "필름",
    "필수",
    "필요",
    "필자",
    "필통",
    "핑계",
    "하느님",
    "하늘",
    "하드웨어",
    "하룻밤",
    "하반기",
    "하숙집",
    "하순",
    "하여튼",
    "하지만",
    "하천",
    "하품",
    "하필",
    "학과",
    "학교",
    "학급",
    "학기",
    "학년",
    "학력",
    "학번",
    "학부모",
    "학비",
    "학생",
    "학술",
    "학습",
    "학용품",
    "학원",
    "학위",
    "학자",
    "학점",
    "한계",
    "한글",
    "한꺼번에",
    "한낮",
    "한눈",
    "한동안",
    "한때",
    "한라산",
    "한마디",
    "한문",
    "한번",
    "한복",
    "한식",
    "한여름",
    "한쪽",
    "할머니",
    "할아버지",
    "할인",
    "함께",
    "함부로",
    "합격",
    "합리적",
    "항공",
    "항구",
    "항상",
    "항의",
    "해결",
    "해군",
    "해답",
    "해당",
    "해물",
    "해석",
    "해설",
    "해수욕장",
    "해안",
    "핵심",
    "핸드백",
    "햄버거",
    "햇볕",
    "햇살",
    "행동",
    "행복",
    "행사",
    "행운",
    "행위",
    "향기",
    "향상",
    "향수",
    "허락",
    "허용",
    "헬기",
    "현관",
    "현금",
    "현대",
    "현상",
    "현실",
    "현장",
    "현재",
    "현지",
    "혈액",
    "협력",
    "형부",
    "형사",
    "형수",
    "형식",
    "형제",
    "형태",
    "형편",
    "혜택",
    "호기심",
    "호남",
    "호랑이",
    "호박",
    "호텔",
    "호흡",
    "혹시",
    "홀로",
    "홈페이지",
    "홍보",
    "홍수",
    "홍차",
    "화면",
    "화분",
    "화살",
    "화요일",
    "화장",
    "화학",
    "확보",
    "확인",
    "확장",
    "확정",
    "환갑",
    "환경",
    "환영",
    "환율",
    "환자",
    "활기",
    "활동",
    "활발히",
    "활용",
    "활짝",
    "회견",
    "회관",
    "회복",
    "회색",
    "회원",
    "회장",
    "회전",
    "횟수",
    "횡단보도",
    "효율적",
    "후반",
    "후춧가루",
    "훈련",
    "훨씬",
    "휴식",
    "휴일",
    "흉내",
    "흐름",
    "흑백",
    "흑인",
    "흔적",
    "흔히",
    "흥미",
    "흥분",
    "희곡",
    "희망",
    "희생",
    "흰색",
    "힘껏"
]

},{}],16:[function(require,module,exports){
module.exports=[
    "ábaco",
    "abdomen",
    "abeja",
    "abierto",
    "abogado",
    "abono",
    "aborto",
    "abrazo",
    "abrir",
    "abuelo",
    "abuso",
    "acabar",
    "academia",
    "acceso",
    "acción",
    "aceite",
    "acelga",
    "acento",
    "aceptar",
    "ácido",
    "aclarar",
    "acné",
    "acoger",
    "acoso",
    "activo",
    "acto",
    "actriz",
    "actuar",
    "acudir",
    "acuerdo",
    "acusar",
    "adicto",
    "admitir",
    "adoptar",
    "adorno",
    "aduana",
    "adulto",
    "aéreo",
    "afectar",
    "afición",
    "afinar",
    "afirmar",
    "ágil",
    "agitar",
    "agonía",
    "agosto",
    "agotar",
    "agregar",
    "agrio",
    "agua",
    "agudo",
    "águila",
    "aguja",
    "ahogo",
    "ahorro",
    "aire",
    "aislar",
    "ajedrez",
    "ajeno",
    "ajuste",
    "alacrán",
    "alambre",
    "alarma",
    "alba",
    "álbum",
    "alcalde",
    "aldea",
    "alegre",
    "alejar",
    "alerta",
    "aleta",
    "alfiler",
    "alga",
    "algodón",
    "aliado",
    "aliento",
    "alivio",
    "alma",
    "almeja",
    "almíbar",
    "altar",
    "alteza",
    "altivo",
    "alto",
    "altura",
    "alumno",
    "alzar",
    "amable",
    "amante",
    "amapola",
    "amargo",
    "amasar",
    "ámbar",
    "ámbito",
    "ameno",
    "amigo",
    "amistad",
    "amor",
    "amparo",
    "amplio",
    "ancho",
    "anciano",
    "ancla",
    "andar",
    "andén",
    "anemia",
    "ángulo",
    "anillo",
    "ánimo",
    "anís",
    "anotar",
    "antena",
    "antiguo",
    "antojo",
    "anual",
    "anular",
    "anuncio",
    "añadir",
    "añejo",
    "año",
    "apagar",
    "aparato",
    "apetito",
    "apio",
    "aplicar",
    "apodo",
    "aporte",
    "apoyo",
    "aprender",
    "aprobar",
    "apuesta",
    "apuro",
    "arado",
    "araña",
    "arar",
    "árbitro",
    "árbol",
    "arbusto",
    "archivo",
    "arco",
    "arder",
    "ardilla",
    "arduo",
    "área",
    "árido",
    "aries",
    "armonía",
    "arnés",
    "aroma",
    "arpa",
    "arpón",
    "arreglo",
    "arroz",
    "arruga",
    "arte",
    "artista",
    "asa",
    "asado",
    "asalto",
    "ascenso",
    "asegurar",
    "aseo",
    "asesor",
    "asiento",
    "asilo",
    "asistir",
    "asno",
    "asombro",
    "áspero",
    "astilla",
    "astro",
    "astuto",
    "asumir",
    "asunto",
    "atajo",
    "ataque",
    "atar",
    "atento",
    "ateo",
    "ático",
    "atleta",
    "átomo",
    "atraer",
    "atroz",
    "atún",
    "audaz",
    "audio",
    "auge",
    "aula",
    "aumento",
    "ausente",
    "autor",
    "aval",
    "avance",
    "avaro",
    "ave",
    "avellana",
    "avena",
    "avestruz",
    "avión",
    "aviso",
    "ayer",
    "ayuda",
    "ayuno",
    "azafrán",
    "azar",
    "azote",
    "azúcar",
    "azufre",
    "azul",
    "baba",
    "babor",
    "bache",
    "bahía",
    "baile",
    "bajar",
    "balanza",
    "balcón",
    "balde",
    "bambú",
    "banco",
    "banda",
    "baño",
    "barba",
    "barco",
    "barniz",
    "barro",
    "báscula",
    "bastón",
    "basura",
    "batalla",
    "batería",
    "batir",
    "batuta",
    "baúl",
    "bazar",
    "bebé",
    "bebida",
    "bello",
    "besar",
    "beso",
    "bestia",
    "bicho",
    "bien",
    "bingo",
    "blanco",
    "bloque",
    "blusa",
    "boa",
    "bobina",
    "bobo",
    "boca",
    "bocina",
    "boda",
    "bodega",
    "boina",
    "bola",
    "bolero",
    "bolsa",
    "bomba",
    "bondad",
    "bonito",
    "bono",
    "bonsái",
    "borde",
    "borrar",
    "bosque",
    "bote",
    "botín",
    "bóveda",
    "bozal",
    "bravo",
    "brazo",
    "brecha",
    "breve",
    "brillo",
    "brinco",
    "brisa",
    "broca",
    "broma",
    "bronce",
    "brote",
    "bruja",
    "brusco",
    "bruto",
    "buceo",
    "bucle",
    "bueno",
    "buey",
    "bufanda",
    "bufón",
    "búho",
    "buitre",
    "bulto",
    "burbuja",
    "burla",
    "burro",
    "buscar",
    "butaca",
    "buzón",
    "caballo",
    "cabeza",
    "cabina",
    "cabra",
    "cacao",
    "cadáver",
    "cadena",
    "caer",
    "café",
    "caída",
    "caimán",
    "caja",
    "cajón",
    "cal",
    "calamar",
    "calcio",
    "caldo",
    "calidad",
    "calle",
    "calma",
    "calor",
    "calvo",
    "cama",
    "cambio",
    "camello",
    "camino",
    "campo",
    "cáncer",
    "candil",
    "canela",
    "canguro",
    "canica",
    "canto",
    "caña",
    "cañón",
    "caoba",
    "caos",
    "capaz",
    "capitán",
    "capote",
    "captar",
    "capucha",
    "cara",
    "carbón",
    "cárcel",
    "careta",
    "carga",
    "cariño",
    "carne",
    "carpeta",
    "carro",
    "carta",
    "casa",
    "casco",
    "casero",
    "caspa",
    "castor",
    "catorce",
    "catre",
    "caudal",
    "causa",
    "cazo",
    "cebolla",
    "ceder",
    "cedro",
    "celda",
    "célebre",
    "celoso",
    "célula",
    "cemento",
    "ceniza",
    "centro",
    "cerca",
    "cerdo",
    "cereza",
    "cero",
    "cerrar",
    "certeza",
    "césped",
    "cetro",
    "chacal",
    "chaleco",
    "champú",
    "chancla",
    "chapa",
    "charla",
    "chico",
    "chiste",
    "chivo",
    "choque",
    "choza",
    "chuleta",
    "chupar",
    "ciclón",
    "ciego",
    "cielo",
    "cien",
    "cierto",
    "cifra",
    "cigarro",
    "cima",
    "cinco",
    "cine",
    "cinta",
    "ciprés",
    "circo",
    "ciruela",
    "cisne",
    "cita",
    "ciudad",
    "clamor",
    "clan",
    "claro",
    "clase",
    "clave",
    "cliente",
    "clima",
    "clínica",
    "cobre",
    "cocción",
    "cochino",
    "cocina",
    "coco",
    "código",
    "codo",
    "cofre",
    "coger",
    "cohete",
    "cojín",
    "cojo",
    "cola",
    "colcha",
    "colegio",
    "colgar",
    "colina",
    "collar",
    "colmo",
    "columna",
    "combate",
    "comer",
    "comida",
    "cómodo",
    "compra",
    "conde",
    "conejo",
    "conga",
    "conocer",
    "consejo",
    "contar",
    "copa",
    "copia",
    "corazón",
    "corbata",
    "corcho",
    "cordón",
    "corona",
    "correr",
    "coser",
    "cosmos",
    "costa",
    "cráneo",
    "cráter",
    "crear",
    "crecer",
    "creído",
    "crema",
    "cría",
    "crimen",
    "cripta",
    "crisis",
    "cromo",
    "crónica",
    "croqueta",
    "crudo",
    "cruz",
    "cuadro",
    "cuarto",
    "cuatro",
    "cubo",
    "cubrir",
    "cuchara",
    "cuello",
    "cuento",
    "cuerda",
    "cuesta",
    "cueva",
    "cuidar",
    "culebra",
    "culpa",
    "culto",
    "cumbre",
    "cumplir",
    "cuna",
    "cuneta",
    "cuota",
    "cupón",
    "cúpula",
    "curar",
    "curioso",
    "curso",
    "curva",
    "cutis",
    "dama",
    "danza",
    "dar",
    "dardo",
    "dátil",
    "deber",
    "débil",
    "década",
    "decir",
    "dedo",
    "defensa",
    "definir",
    "dejar",
    "delfín",
    "delgado",
    "delito",
    "demora",
    "denso",
    "dental",
    "deporte",
    "derecho",
    "derrota",
    "desayuno",
    "deseo",
    "desfile",
    "desnudo",
    "destino",
    "desvío",
    "detalle",
    "detener",
    "deuda",
    "día",
    "diablo",
    "diadema",
    "diamante",
    "diana",
    "diario",
    "dibujo",
    "dictar",
    "diente",
    "dieta",
    "diez",
    "difícil",
    "digno",
    "dilema",
    "diluir",
    "dinero",
    "directo",
    "dirigir",
    "disco",
    "diseño",
    "disfraz",
    "diva",
    "divino",
    "doble",
    "doce",
    "dolor",
    "domingo",
    "don",
    "donar",
    "dorado",
    "dormir",
    "dorso",
    "dos",
    "dosis",
    "dragón",
    "droga",
    "ducha",
    "duda",
    "duelo",
    "dueño",
    "dulce",
    "dúo",
    "duque",
    "durar",
    "dureza",
    "duro",
    "ébano",
    "ebrio",
    "echar",
    "eco",
    "ecuador",
    "edad",
    "edición",
    "edificio",
    "editor",
    "educar",
    "efecto",
    "eficaz",
    "eje",
    "ejemplo",
    "elefante",
    "elegir",
    "elemento",
    "elevar",
    "elipse",
    "élite",
    "elixir",
    "elogio",
    "eludir",
    "embudo",
    "emitir",
    "emoción",
    "empate",
    "empeño",
    "empleo",
    "empresa",
    "enano",
    "encargo",
    "enchufe",
    "encía",
    "enemigo",
    "enero",
    "enfado",
    "enfermo",
    "engaño",
    "enigma",
    "enlace",
    "enorme",
    "enredo",
    "ensayo",
    "enseñar",
    "entero",
    "entrar",
    "envase",
    "envío",
    "época",
    "equipo",
    "erizo",
    "escala",
    "escena",
    "escolar",
    "escribir",
    "escudo",
    "esencia",
    "esfera",
    "esfuerzo",
    "espada",
    "espejo",
    "espía",
    "esposa",
    "espuma",
    "esquí",
    "estar",
    "este",
    "estilo",
    "estufa",
    "etapa",
    "eterno",
    "ética",
    "etnia",
    "evadir",
    "evaluar",
    "evento",
    "evitar",
    "exacto",
    "examen",
    "exceso",
    "excusa",
    "exento",
    "exigir",
    "exilio",
    "existir",
    "éxito",
    "experto",
    "explicar",
    "exponer",
    "extremo",
    "fábrica",
    "fábula",
    "fachada",
    "fácil",
    "factor",
    "faena",
    "faja",
    "falda",
    "fallo",
    "falso",
    "faltar",
    "fama",
    "familia",
    "famoso",
    "faraón",
    "farmacia",
    "farol",
    "farsa",
    "fase",
    "fatiga",
    "fauna",
    "favor",
    "fax",
    "febrero",
    "fecha",
    "feliz",
    "feo",
    "feria",
    "feroz",
    "fértil",
    "fervor",
    "festín",
    "fiable",
    "fianza",
    "fiar",
    "fibra",
    "ficción",
    "ficha",
    "fideo",
    "fiebre",
    "fiel",
    "fiera",
    "fiesta",
    "figura",
    "fijar",
    "fijo",
    "fila",
    "filete",
    "filial",
    "filtro",
    "fin",
    "finca",
    "fingir",
    "finito",
    "firma",
    "flaco",
    "flauta",
    "flecha",
    "flor",
    "flota",
    "fluir",
    "flujo",
    "flúor",
    "fobia",
    "foca",
    "fogata",
    "fogón",
    "folio",
    "folleto",
    "fondo",
    "forma",
    "forro",
    "fortuna",
    "forzar",
    "fosa",
    "foto",
    "fracaso",
    "frágil",
    "franja",
    "frase",
    "fraude",
    "freír",
    "freno",
    "fresa",
    "frío",
    "frito",
    "fruta",
    "fuego",
    "fuente",
    "fuerza",
    "fuga",
    "fumar",
    "función",
    "funda",
    "furgón",
    "furia",
    "fusil",
    "fútbol",
    "futuro",
    "gacela",
    "gafas",
    "gaita",
    "gajo",
    "gala",
    "galería",
    "gallo",
    "gamba",
    "ganar",
    "gancho",
    "ganga",
    "ganso",
    "garaje",
    "garza",
    "gasolina",
    "gastar",
    "gato",
    "gavilán",
    "gemelo",
    "gemir",
    "gen",
    "género",
    "genio",
    "gente",
    "geranio",
    "gerente",
    "germen",
    "gesto",
    "gigante",
    "gimnasio",
    "girar",
    "giro",
    "glaciar",
    "globo",
    "gloria",
    "gol",
    "golfo",
    "goloso",
    "golpe",
    "goma",
    "gordo",
    "gorila",
    "gorra",
    "gota",
    "goteo",
    "gozar",
    "grada",
    "gráfico",
    "grano",
    "grasa",
    "gratis",
    "grave",
    "grieta",
    "grillo",
    "gripe",
    "gris",
    "grito",
    "grosor",
    "grúa",
    "grueso",
    "grumo",
    "grupo",
    "guante",
    "guapo",
    "guardia",
    "guerra",
    "guía",
    "guiño",
    "guion",
    "guiso",
    "guitarra",
    "gusano",
    "gustar",
    "haber",
    "hábil",
    "hablar",
    "hacer",
    "hacha",
    "hada",
    "hallar",
    "hamaca",
    "harina",
    "haz",
    "hazaña",
    "hebilla",
    "hebra",
    "hecho",
    "helado",
    "helio",
    "hembra",
    "herir",
    "hermano",
    "héroe",
    "hervir",
    "hielo",
    "hierro",
    "hígado",
    "higiene",
    "hijo",
    "himno",
    "historia",
    "hocico",
    "hogar",
    "hoguera",
    "hoja",
    "hombre",
    "hongo",
    "honor",
    "honra",
    "hora",
    "hormiga",
    "horno",
    "hostil",
    "hoyo",
    "hueco",
    "huelga",
    "huerta",
    "hueso",
    "huevo",
    "huida",
    "huir",
    "humano",
    "húmedo",
    "humilde",
    "humo",
    "hundir",
    "huracán",
    "hurto",
    "icono",
    "ideal",
    "idioma",
    "ídolo",
    "iglesia",
    "iglú",
    "igual",
    "ilegal",
    "ilusión",
    "imagen",
    "imán",
    "imitar",
    "impar",
    "imperio",
    "imponer",
    "impulso",
    "incapaz",
    "índice",
    "inerte",
    "infiel",
    "informe",
    "ingenio",
    "inicio",
    "inmenso",
    "inmune",
    "innato",
    "insecto",
    "instante",
    "interés",
    "íntimo",
    "intuir",
    "inútil",
    "invierno",
    "ira",
    "iris",
    "ironía",
    "isla",
    "islote",
    "jabalí",
    "jabón",
    "jamón",
    "jarabe",
    "jardín",
    "jarra",
    "jaula",
    "jazmín",
    "jefe",
    "jeringa",
    "jinete",
    "jornada",
    "joroba",
    "joven",
    "joya",
    "juerga",
    "jueves",
    "juez",
    "jugador",
    "jugo",
    "juguete",
    "juicio",
    "junco",
    "jungla",
    "junio",
    "juntar",
    "júpiter",
    "jurar",
    "justo",
    "juvenil",
    "juzgar",
    "kilo",
    "koala",
    "labio",
    "lacio",
    "lacra",
    "lado",
    "ladrón",
    "lagarto",
    "lágrima",
    "laguna",
    "laico",
    "lamer",
    "lámina",
    "lámpara",
    "lana",
    "lancha",
    "langosta",
    "lanza",
    "lápiz",
    "largo",
    "larva",
    "lástima",
    "lata",
    "látex",
    "latir",
    "laurel",
    "lavar",
    "lazo",
    "leal",
    "lección",
    "leche",
    "lector",
    "leer",
    "legión",
    "legumbre",
    "lejano",
    "lengua",
    "lento",
    "leña",
    "león",
    "leopardo",
    "lesión",
    "letal",
    "letra",
    "leve",
    "leyenda",
    "libertad",
    "libro",
    "licor",
    "líder",
    "lidiar",
    "lienzo",
    "liga",
    "ligero",
    "lima",
    "límite",
    "limón",
    "limpio",
    "lince",
    "lindo",
    "línea",
    "lingote",
    "lino",
    "linterna",
    "líquido",
    "liso",
    "lista",
    "litera",
    "litio",
    "litro",
    "llaga",
    "llama",
    "llanto",
    "llave",
    "llegar",
    "llenar",
    "llevar",
    "llorar",
    "llover",
    "lluvia",
    "lobo",
    "loción",
    "loco",
    "locura",
    "lógica",
    "logro",
    "lombriz",
    "lomo",
    "lonja",
    "lote",
    "lucha",
    "lucir",
    "lugar",
    "lujo",
    "luna",
    "lunes",
    "lupa",
    "lustro",
    "luto",
    "luz",
    "maceta",
    "macho",
    "madera",
    "madre",
    "maduro",
    "maestro",
    "mafia",
    "magia",
    "mago",
    "maíz",
    "maldad",
    "maleta",
    "malla",
    "malo",
    "mamá",
    "mambo",
    "mamut",
    "manco",
    "mando",
    "manejar",
    "manga",
    "maniquí",
    "manjar",
    "mano",
    "manso",
    "manta",
    "mañana",
    "mapa",
    "máquina",
    "mar",
    "marco",
    "marea",
    "marfil",
    "margen",
    "marido",
    "mármol",
    "marrón",
    "martes",
    "marzo",
    "masa",
    "máscara",
    "masivo",
    "matar",
    "materia",
    "matiz",
    "matriz",
    "máximo",
    "mayor",
    "mazorca",
    "mecha",
    "medalla",
    "medio",
    "médula",
    "mejilla",
    "mejor",
    "melena",
    "melón",
    "memoria",
    "menor",
    "mensaje",
    "mente",
    "menú",
    "mercado",
    "merengue",
    "mérito",
    "mes",
    "mesón",
    "meta",
    "meter",
    "método",
    "metro",
    "mezcla",
    "miedo",
    "miel",
    "miembro",
    "miga",
    "mil",
    "milagro",
    "militar",
    "millón",
    "mimo",
    "mina",
    "minero",
    "mínimo",
    "minuto",
    "miope",
    "mirar",
    "misa",
    "miseria",
    "misil",
    "mismo",
    "mitad",
    "mito",
    "mochila",
    "moción",
    "moda",
    "modelo",
    "moho",
    "mojar",
    "molde",
    "moler",
    "molino",
    "momento",
    "momia",
    "monarca",
    "moneda",
    "monja",
    "monto",
    "moño",
    "morada",
    "morder",
    "moreno",
    "morir",
    "morro",
    "morsa",
    "mortal",
    "mosca",
    "mostrar",
    "motivo",
    "mover",
    "móvil",
    "mozo",
    "mucho",
    "mudar",
    "mueble",
    "muela",
    "muerte",
    "muestra",
    "mugre",
    "mujer",
    "mula",
    "muleta",
    "multa",
    "mundo",
    "muñeca",
    "mural",
    "muro",
    "músculo",
    "museo",
    "musgo",
    "música",
    "muslo",
    "nácar",
    "nación",
    "nadar",
    "naipe",
    "naranja",
    "nariz",
    "narrar",
    "nasal",
    "natal",
    "nativo",
    "natural",
    "náusea",
    "naval",
    "nave",
    "navidad",
    "necio",
    "néctar",
    "negar",
    "negocio",
    "negro",
    "neón",
    "nervio",
    "neto",
    "neutro",
    "nevar",
    "nevera",
    "nicho",
    "nido",
    "niebla",
    "nieto",
    "niñez",
    "niño",
    "nítido",
    "nivel",
    "nobleza",
    "noche",
    "nómina",
    "noria",
    "norma",
    "norte",
    "nota",
    "noticia",
    "novato",
    "novela",
    "novio",
    "nube",
    "nuca",
    "núcleo",
    "nudillo",
    "nudo",
    "nuera",
    "nueve",
    "nuez",
    "nulo",
    "número",
    "nutria",
    "oasis",
    "obeso",
    "obispo",
    "objeto",
    "obra",
    "obrero",
    "observar",
    "obtener",
    "obvio",
    "oca",
    "ocaso",
    "océano",
    "ochenta",
    "ocho",
    "ocio",
    "ocre",
    "octavo",
    "octubre",
    "oculto",
    "ocupar",
    "ocurrir",
    "odiar",
    "odio",
    "odisea",
    "oeste",
    "ofensa",
    "oferta",
    "oficio",
    "ofrecer",
    "ogro",
    "oído",
    "oír",
    "ojo",
    "ola",
    "oleada",
    "olfato",
    "olivo",
    "olla",
    "olmo",
    "olor",
    "olvido",
    "ombligo",
    "onda",
    "onza",
    "opaco",
    "opción",
    "ópera",
    "opinar",
    "oponer",
    "optar",
    "óptica",
    "opuesto",
    "oración",
    "orador",
    "oral",
    "órbita",
    "orca",
    "orden",
    "oreja",
    "órgano",
    "orgía",
    "orgullo",
    "oriente",
    "origen",
    "orilla",
    "oro",
    "orquesta",
    "oruga",
    "osadía",
    "oscuro",
    "osezno",
    "oso",
    "ostra",
    "otoño",
    "otro",
    "oveja",
    "óvulo",
    "óxido",
    "oxígeno",
    "oyente",
    "ozono",
    "pacto",
    "padre",
    "paella",
    "página",
    "pago",
    "país",
    "pájaro",
    "palabra",
    "palco",
    "paleta",
    "pálido",
    "palma",
    "paloma",
    "palpar",
    "pan",
    "panal",
    "pánico",
    "pantera",
    "pañuelo",
    "papá",
    "papel",
    "papilla",
    "paquete",
    "parar",
    "parcela",
    "pared",
    "parir",
    "paro",
    "párpado",
    "parque",
    "párrafo",
    "parte",
    "pasar",
    "paseo",
    "pasión",
    "paso",
    "pasta",
    "pata",
    "patio",
    "patria",
    "pausa",
    "pauta",
    "pavo",
    "payaso",
    "peatón",
    "pecado",
    "pecera",
    "pecho",
    "pedal",
    "pedir",
    "pegar",
    "peine",
    "pelar",
    "peldaño",
    "pelea",
    "peligro",
    "pellejo",
    "pelo",
    "peluca",
    "pena",
    "pensar",
    "peñón",
    "peón",
    "peor",
    "pepino",
    "pequeño",
    "pera",
    "percha",
    "perder",
    "pereza",
    "perfil",
    "perico",
    "perla",
    "permiso",
    "perro",
    "persona",
    "pesa",
    "pesca",
    "pésimo",
    "pestaña",
    "pétalo",
    "petróleo",
    "pez",
    "pezuña",
    "picar",
    "pichón",
    "pie",
    "piedra",
    "pierna",
    "pieza",
    "pijama",
    "pilar",
    "piloto",
    "pimienta",
    "pino",
    "pintor",
    "pinza",
    "piña",
    "piojo",
    "pipa",
    "pirata",
    "pisar",
    "piscina",
    "piso",
    "pista",
    "pitón",
    "pizca",
    "placa",
    "plan",
    "plata",
    "playa",
    "plaza",
    "pleito",
    "pleno",
    "plomo",
    "pluma",
    "plural",
    "pobre",
    "poco",
    "poder",
    "podio",
    "poema",
    "poesía",
    "poeta",
    "polen",
    "policía",
    "pollo",
    "polvo",
    "pomada",
    "pomelo",
    "pomo",
    "pompa",
    "poner",
    "porción",
    "portal",
    "posada",
    "poseer",
    "posible",
    "poste",
    "potencia",
    "potro",
    "pozo",
    "prado",
    "precoz",
    "pregunta",
    "premio",
    "prensa",
    "preso",
    "previo",
    "primo",
    "príncipe",
    "prisión",
    "privar",
    "proa",
    "probar",
    "proceso",
    "producto",
    "proeza",
    "profesor",
    "programa",
    "prole",
    "promesa",
    "pronto",
    "propio",
    "próximo",
    "prueba",
    "público",
    "puchero",
    "pudor",
    "pueblo",
    "puerta",
    "puesto",
    "pulga",
    "pulir",
    "pulmón",
    "pulpo",
    "pulso",
    "puma",
    "punto",
    "puñal",
    "puño",
    "pupa",
    "pupila",
    "puré",
    "quedar",
    "queja",
    "quemar",
    "querer",
    "queso",
    "quieto",
    "química",
    "quince",
    "quitar",
    "rábano",
    "rabia",
    "rabo",
    "ración",
    "radical",
    "raíz",
    "rama",
    "rampa",
    "rancho",
    "rango",
    "rapaz",
    "rápido",
    "rapto",
    "rasgo",
    "raspa",
    "rato",
    "rayo",
    "raza",
    "razón",
    "reacción",
    "realidad",
    "rebaño",
    "rebote",
    "recaer",
    "receta",
    "rechazo",
    "recoger",
    "recreo",
    "recto",
    "recurso",
    "red",
    "redondo",
    "reducir",
    "reflejo",
    "reforma",
    "refrán",
    "refugio",
    "regalo",
    "regir",
    "regla",
    "regreso",
    "rehén",
    "reino",
    "reír",
    "reja",
    "relato",
    "relevo",
    "relieve",
    "relleno",
    "reloj",
    "remar",
    "remedio",
    "remo",
    "rencor",
    "rendir",
    "renta",
    "reparto",
    "repetir",
    "reposo",
    "reptil",
    "res",
    "rescate",
    "resina",
    "respeto",
    "resto",
    "resumen",
    "retiro",
    "retorno",
    "retrato",
    "reunir",
    "revés",
    "revista",
    "rey",
    "rezar",
    "rico",
    "riego",
    "rienda",
    "riesgo",
    "rifa",
    "rígido",
    "rigor",
    "rincón",
    "riñón",
    "río",
    "riqueza",
    "risa",
    "ritmo",
    "rito",
    "rizo",
    "roble",
    "roce",
    "rociar",
    "rodar",
    "rodeo",
    "rodilla",
    "roer",
    "rojizo",
    "rojo",
    "romero",
    "romper",
    "ron",
    "ronco",
    "ronda",
    "ropa",
    "ropero",
    "rosa",
    "rosca",
    "rostro",
    "rotar",
    "rubí",
    "rubor",
    "rudo",
    "rueda",
    "rugir",
    "ruido",
    "ruina",
    "ruleta",
    "rulo",
    "rumbo",
    "rumor",
    "ruptura",
    "ruta",
    "rutina",
    "sábado",
    "saber",
    "sabio",
    "sable",
    "sacar",
    "sagaz",
    "sagrado",
    "sala",
    "saldo",
    "salero",
    "salir",
    "salmón",
    "salón",
    "salsa",
    "salto",
    "salud",
    "salvar",
    "samba",
    "sanción",
    "sandía",
    "sanear",
    "sangre",
    "sanidad",
    "sano",
    "santo",
    "sapo",
    "saque",
    "sardina",
    "sartén",
    "sastre",
    "satán",
    "sauna",
    "saxofón",
    "sección",
    "seco",
    "secreto",
    "secta",
    "sed",
    "seguir",
    "seis",
    "sello",
    "selva",
    "semana",
    "semilla",
    "senda",
    "sensor",
    "señal",
    "señor",
    "separar",
    "sepia",
    "sequía",
    "ser",
    "serie",
    "sermón",
    "servir",
    "sesenta",
    "sesión",
    "seta",
    "setenta",
    "severo",
    "sexo",
    "sexto",
    "sidra",
    "siesta",
    "siete",
    "siglo",
    "signo",
    "sílaba",
    "silbar",
    "silencio",
    "silla",
    "símbolo",
    "simio",
    "sirena",
    "sistema",
    "sitio",
    "situar",
    "sobre",
    "socio",
    "sodio",
    "sol",
    "solapa",
    "soldado",
    "soledad",
    "sólido",
    "soltar",
    "solución",
    "sombra",
    "sondeo",
    "sonido",
    "sonoro",
    "sonrisa",
    "sopa",
    "soplar",
    "soporte",
    "sordo",
    "sorpresa",
    "sorteo",
    "sostén",
    "sótano",
    "suave",
    "subir",
    "suceso",
    "sudor",
    "suegra",
    "suelo",
    "sueño",
    "suerte",
    "sufrir",
    "sujeto",
    "sultán",
    "sumar",
    "superar",
    "suplir",
    "suponer",
    "supremo",
    "sur",
    "surco",
    "sureño",
    "surgir",
    "susto",
    "sutil",
    "tabaco",
    "tabique",
    "tabla",
    "tabú",
    "taco",
    "tacto",
    "tajo",
    "talar",
    "talco",
    "talento",
    "talla",
    "talón",
    "tamaño",
    "tambor",
    "tango",
    "tanque",
    "tapa",
    "tapete",
    "tapia",
    "tapón",
    "taquilla",
    "tarde",
    "tarea",
    "tarifa",
    "tarjeta",
    "tarot",
    "tarro",
    "tarta",
    "tatuaje",
    "tauro",
    "taza",
    "tazón",
    "teatro",
    "techo",
    "tecla",
    "técnica",
    "tejado",
    "tejer",
    "tejido",
    "tela",
    "teléfono",
    "tema",
    "temor",
    "templo",
    "tenaz",
    "tender",
    "tener",
    "tenis",
    "tenso",
    "teoría",
    "terapia",
    "terco",
    "término",
    "ternura",
    "terror",
    "tesis",
    "tesoro",
    "testigo",
    "tetera",
    "texto",
    "tez",
    "tibio",
    "tiburón",
    "tiempo",
    "tienda",
    "tierra",
    "tieso",
    "tigre",
    "tijera",
    "tilde",
    "timbre",
    "tímido",
    "timo",
    "tinta",
    "tío",
    "típico",
    "tipo",
    "tira",
    "tirón",
    "titán",
    "títere",
    "título",
    "tiza",
    "toalla",
    "tobillo",
    "tocar",
    "tocino",
    "todo",
    "toga",
    "toldo",
    "tomar",
    "tono",
    "tonto",
    "topar",
    "tope",
    "toque",
    "tórax",
    "torero",
    "tormenta",
    "torneo",
    "toro",
    "torpedo",
    "torre",
    "torso",
    "tortuga",
    "tos",
    "tosco",
    "toser",
    "tóxico",
    "trabajo",
    "tractor",
    "traer",
    "tráfico",
    "trago",
    "traje",
    "tramo",
    "trance",
    "trato",
    "trauma",
    "trazar",
    "trébol",
    "tregua",
    "treinta",
    "tren",
    "trepar",
    "tres",
    "tribu",
    "trigo",
    "tripa",
    "triste",
    "triunfo",
    "trofeo",
    "trompa",
    "tronco",
    "tropa",
    "trote",
    "trozo",
    "truco",
    "trueno",
    "trufa",
    "tubería",
    "tubo",
    "tuerto",
    "tumba",
    "tumor",
    "túnel",
    "túnica",
    "turbina",
    "turismo",
    "turno",
    "tutor",
    "ubicar",
    "úlcera",
    "umbral",
    "unidad",
    "unir",
    "universo",
    "uno",
    "untar",
    "uña",
    "urbano",
    "urbe",
    "urgente",
    "urna",
    "usar",
    "usuario",
    "útil",
    "utopía",
    "uva",
    "vaca",
    "vacío",
    "vacuna",
    "vagar",
    "vago",
    "vaina",
    "vajilla",
    "vale",
    "válido",
    "valle",
    "valor",
    "válvula",
    "vampiro",
    "vara",
    "variar",
    "varón",
    "vaso",
    "vecino",
    "vector",
    "vehículo",
    "veinte",
    "vejez",
    "vela",
    "velero",
    "veloz",
    "vena",
    "vencer",
    "venda",
    "veneno",
    "vengar",
    "venir",
    "venta",
    "venus",
    "ver",
    "verano",
    "verbo",
    "verde",
    "vereda",
    "verja",
    "verso",
    "verter",
    "vía",
    "viaje",
    "vibrar",
    "vicio",
    "víctima",
    "vida",
    "vídeo",
    "vidrio",
    "viejo",
    "viernes",
    "vigor",
    "vil",
    "villa",
    "vinagre",
    "vino",
    "viñedo",
    "violín",
    "viral",
    "virgo",
    "virtud",
    "visor",
    "víspera",
    "vista",
    "vitamina",
    "viudo",
    "vivaz",
    "vivero",
    "vivir",
    "vivo",
    "volcán",
    "volumen",
    "volver",
    "voraz",
    "votar",
    "voto",
    "voz",
    "vuelo",
    "vulgar",
    "yacer",
    "yate",
    "yegua",
    "yema",
    "yerno",
    "yeso",
    "yodo",
    "yoga",
    "yogur",
    "zafiro",
    "zanja",
    "zapato",
    "zarza",
    "zona",
    "zorro",
    "zumo",
    "zurdo"
]

},{}],17:[function(require,module,exports){
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

},{"inherits":21,"safe-buffer":30,"stream":70,"string_decoder":71}],18:[function(require,module,exports){
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

},{"cipher-base":17,"inherits":21,"md5.js":22,"ripemd160":29,"sha.js":32}],19:[function(require,module,exports){
var MD5 = require('md5.js')

module.exports = function (buffer) {
  return new MD5().update(buffer).digest()
}

},{"md5.js":22}],20:[function(require,module,exports){
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

},{"inherits":21,"safe-buffer":30,"stream":70}],21:[function(require,module,exports){
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

},{}],22:[function(require,module,exports){
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
},{"buffer":44,"hash-base":20,"inherits":21}],23:[function(require,module,exports){
exports.pbkdf2 = require('./lib/async')
exports.pbkdf2Sync = require('./lib/sync')

},{"./lib/async":24,"./lib/sync":27}],24:[function(require,module,exports){
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
},{"./default-encoding":25,"./precondition":26,"./sync":27,"_process":53,"safe-buffer":30}],25:[function(require,module,exports){
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
},{"_process":53}],26:[function(require,module,exports){
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
},{"../../../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js":49}],27:[function(require,module,exports){
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

},{"./default-encoding":25,"./precondition":26,"create-hash/md5":19,"ripemd160":29,"safe-buffer":30,"sha.js":32}],28:[function(require,module,exports){
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
},{"_process":53,"safe-buffer":30}],29:[function(require,module,exports){
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

},{"buffer":44,"hash-base":20,"inherits":21}],30:[function(require,module,exports){
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

},{"buffer":44}],31:[function(require,module,exports){
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

},{"safe-buffer":30}],32:[function(require,module,exports){
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

},{"./sha":33,"./sha1":34,"./sha224":35,"./sha256":36,"./sha384":37,"./sha512":38}],33:[function(require,module,exports){
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

},{"./hash":31,"inherits":21,"safe-buffer":30}],34:[function(require,module,exports){
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

},{"./hash":31,"inherits":21,"safe-buffer":30}],35:[function(require,module,exports){
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

},{"./hash":31,"./sha256":36,"inherits":21,"safe-buffer":30}],36:[function(require,module,exports){
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

},{"./hash":31,"inherits":21,"safe-buffer":30}],37:[function(require,module,exports){
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

},{"./hash":31,"./sha512":38,"inherits":21,"safe-buffer":30}],38:[function(require,module,exports){
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

},{"./hash":31,"inherits":21,"safe-buffer":30}],39:[function(require,module,exports){
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

},{"pbkdf2":23}],40:[function(require,module,exports){
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
},{"../../../../../nix/store/y42lxz5r3xc8x79ainln7hfv7xlapz6l-node_browserify-16.5.2/lib/node_modules/browserify/node_modules/is-buffer/index.js":49,"bip39":8}],41:[function(require,module,exports){

},{}],42:[function(require,module,exports){
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

},{}],43:[function(require,module,exports){
arguments[4][41][0].apply(exports,arguments)
},{"dup":41}],44:[function(require,module,exports){
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
},{"base64-js":42,"buffer":44,"ieee754":47}],45:[function(require,module,exports){
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
},{"../../is-buffer/index.js":49}],46:[function(require,module,exports){
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

},{}],47:[function(require,module,exports){
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

},{}],48:[function(require,module,exports){
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

},{}],49:[function(require,module,exports){
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

},{}],50:[function(require,module,exports){
var toString = {}.toString;

module.exports = Array.isArray || function (arr) {
  return toString.call(arr) == '[object Array]';
};

},{}],51:[function(require,module,exports){
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
},{"_process":53}],52:[function(require,module,exports){
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
},{"_process":53}],53:[function(require,module,exports){
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

},{}],54:[function(require,module,exports){
module.exports = require('./lib/_stream_duplex.js');

},{"./lib/_stream_duplex.js":55}],55:[function(require,module,exports){
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
},{"./_stream_readable":57,"./_stream_writable":59,"core-util-is":45,"inherits":48,"process-nextick-args":52}],56:[function(require,module,exports){
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
},{"./_stream_transform":58,"core-util-is":45,"inherits":48}],57:[function(require,module,exports){
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
},{"./_stream_duplex":55,"./internal/streams/BufferList":60,"./internal/streams/destroy":61,"./internal/streams/stream":62,"_process":53,"core-util-is":45,"events":46,"inherits":48,"isarray":50,"process-nextick-args":52,"safe-buffer":63,"string_decoder/":64,"util":43}],58:[function(require,module,exports){
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
},{"./_stream_duplex":55,"core-util-is":45,"inherits":48}],59:[function(require,module,exports){
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
},{"./_stream_duplex":55,"./internal/streams/destroy":61,"./internal/streams/stream":62,"_process":53,"core-util-is":45,"inherits":48,"process-nextick-args":52,"safe-buffer":63,"timers":72,"util-deprecate":73}],60:[function(require,module,exports){
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
},{"safe-buffer":63,"util":43}],61:[function(require,module,exports){
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
},{"process-nextick-args":52}],62:[function(require,module,exports){
module.exports = require('events').EventEmitter;

},{"events":46}],63:[function(require,module,exports){
arguments[4][30][0].apply(exports,arguments)
},{"buffer":44,"dup":30}],64:[function(require,module,exports){
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
},{"safe-buffer":63}],65:[function(require,module,exports){
module.exports = require('./readable').PassThrough

},{"./readable":66}],66:[function(require,module,exports){
exports = module.exports = require('./lib/_stream_readable.js');
exports.Stream = exports;
exports.Readable = exports;
exports.Writable = require('./lib/_stream_writable.js');
exports.Duplex = require('./lib/_stream_duplex.js');
exports.Transform = require('./lib/_stream_transform.js');
exports.PassThrough = require('./lib/_stream_passthrough.js');

},{"./lib/_stream_duplex.js":55,"./lib/_stream_passthrough.js":56,"./lib/_stream_readable.js":57,"./lib/_stream_transform.js":58,"./lib/_stream_writable.js":59}],67:[function(require,module,exports){
module.exports = require('./readable').Transform

},{"./readable":66}],68:[function(require,module,exports){
module.exports = require('./lib/_stream_writable.js');

},{"./lib/_stream_writable.js":59}],69:[function(require,module,exports){
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

},{"buffer":44}],70:[function(require,module,exports){
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

},{"events":46,"inherits":48,"readable-stream/duplex.js":54,"readable-stream/passthrough.js":65,"readable-stream/readable.js":66,"readable-stream/transform.js":67,"readable-stream/writable.js":68}],71:[function(require,module,exports){
arguments[4][64][0].apply(exports,arguments)
},{"dup":64,"safe-buffer":69}],72:[function(require,module,exports){
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
},{"process/browser.js":53,"timers":72}],73:[function(require,module,exports){
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
