// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O10_Vuln {
    address public owner;
    address public implementation;

    // [VULN] initialize exposta: se deployer nao inicializar, qualquer um toma ownership.
    function initialize(address newOwner, address initialImpl) external {
        require(owner == address(0), "already initialized");
        owner = newOwner;
        implementation = initialImpl;
    }

    function upgradeTo(address newImpl) external {
        require(msg.sender == owner, "not owner");
        implementation = newImpl;
    }
}
