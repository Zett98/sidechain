"use strict";

const fs = require("fs");
const taquito = require("@taquito/taquito");
const { RpcClient } = require("@taquito/rpc");
const { InMemorySigner } = require("@taquito/signer");

const { TezosToolkit } = taquito;
/**
 * @typedef Input
 * @type {object}
 * @property {string} contract_address
 * @property {string} rpc_node
 */
/** @returns {Input} */
const input = () => JSON.parse(fs.readFileSync(process.stdin.fd));

/**
 * @typedef OutputFinished
 * @type {object}
 * @property {string} root_hash.current_block_hash
 * @property {string} root_hash.current_block_height
 * @property {string} root_hash.current_handles_hash
 * @property {string} root_hash.current_state_hash
 * @property {array} root_hash.current_validators
 * @property {string} vault.known_handles_hash
 * @property {string} vault.used_handles
 * @property {string} vault.vault
 */
/**
 * @typedef OutputError
 * @type {object}
 * @property {"error"} status
 * @property {string} error
 */
/** @param {OutputFinished | OutputError} data */
const output = (data) =>
  fs.writeFileSync(process.stdout.fd, JSON.stringify(data, null, 2));

const finished = (storage) => output(storage);
const error = (error) =>
  output({ status: "error", error: JSON.stringify(error) });

const finality = 10;
(async () => {
  const { rpc_node, contract_address } = input();
  const client = new RpcClient(rpc_node);
  const Tezos = new TezosToolkit(rpc_node);
  const contract = await Tezos.contract.at(contract_address);
  const storage = await contract.storage();
  /* To make sure the storage state is finalized, we query the last
     block and any one of it's operation, and wait till it receives
     `n` confirmations (where n is the minimum blocks needs to
     consider reorg highly unlikely. ie finality) */
  const block = await client.getBlock();
  let operationFromHead = block.operations.flat()[0];
  if (operationFromHead) {
    const operation = await Tezos.operation.createTransactionOperation(
      operationFromHead.hash
    );
    const result = await operation.confirmation(finality);
    if (await result.isInCurrentBranch()) {
    }
  }
  /* However, if operations were empty, reasons being
     1. Stale chain
     2. Node not in archive mode
     ... we go ahead with the storage state */
  finished(storage);
})().catch(error);
