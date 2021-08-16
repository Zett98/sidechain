"use strict";

const fs = require("fs");
const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");

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
 * @property {"current_validators"} current_validators
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

const finished = (current_validators) => output({ current_validators });
const error = (error) =>
  output({ status: "error", error: JSON.stringify(error) });

(async () => {
  const { rpc_node, contract_address } = input();
  const Tezos = new TezosToolkit(rpc_node);
  const contract = await Tezos.contract.at(contract_address);
  const storage = await contract.storage();
  finished(storage.root_hash.current_validators);
})().catch(error);
