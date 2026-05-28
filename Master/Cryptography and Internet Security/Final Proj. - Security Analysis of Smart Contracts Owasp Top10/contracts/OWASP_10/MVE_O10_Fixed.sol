// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O10_Fixed {
    address public owner;
    address public implementation;

    error NotOwner();

    constructor(address initialImpl) {
        // [FIX] Inicializacao feita no constructor para nao deixar janela de takeover.
        owner = msg.sender;
        implementation = initialImpl;
    }

    function upgradeTo(address newImpl) external {
        if (msg.sender != owner) revert NotOwner();
        implementation = newImpl;
    }
}
