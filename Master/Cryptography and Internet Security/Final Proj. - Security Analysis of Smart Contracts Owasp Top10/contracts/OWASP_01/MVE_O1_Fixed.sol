// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O1_Fixed {
    address public owner;
    address payable public treasury;

    error NotOwner();
    error InvalidTreasury();

    modifier onlyOwner() {
        // [FIX] Restringe funcoes criticas ao owner.
        if (msg.sender != owner) revert NotOwner();
        _;
    }

    constructor() {
        owner = msg.sender;
        treasury = payable(msg.sender);
    }

    function deposit() external payable {}

    function setTreasury(address payable newTreasury) external onlyOwner {
        // [FIX] Valida input para evitar treasury invalida.
        if (newTreasury == address(0)) revert InvalidTreasury();
        treasury = newTreasury;
    }

    function emergencyWithdraw() external onlyOwner {
        (bool ok, ) = treasury.call{value: address(this).balance}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
