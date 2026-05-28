// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [FIX] OWASP-08 variante 2: Cross-function reentrancy corrigida
// Duas defesas combinadas:
//   1. CEI (Checks-Effects-Interactions): estado atualizado ANTES da chamada externa.
//   2. Mutex nonReentrant: impede qualquer reentrada durante a execucao de withdraw() ou transfer().
contract MVE_O8_Fixed2 {
    mapping(address => uint256) public balanceOf;
    bool private locked;

    error ReentrantCall();

    modifier nonReentrant() {
        if (locked) revert ReentrantCall();
        locked = true;
        _;
        locked = false;
    }

    function deposit() external payable {
        balanceOf[msg.sender] += msg.value;
    }

    function withdraw(uint256 amount) external nonReentrant {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");

        // [FIX] CEI: saldo decrementado antes da chamada externa.
        balanceOf[msg.sender] -= amount;

        (bool ok, ) = msg.sender.call{value: amount}("");
        require(ok, "transfer failed");
    }

    // [FIX] Mutex impede que transfer() seja invocado durante callback de withdraw().
    function transfer(address to, uint256 amount) external nonReentrant {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");
        balanceOf[msg.sender] -= amount;
        balanceOf[to] += amount;
    }
}
