// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [FIX] OWASP-01 variante 2: Substituir tx.origin por msg.sender
// msg.sender e sempre o chamador imediato — se for um contrato intermedio,
// a verificacao falha, tornando o ataque de phishing impossivel.
contract MVE_O1_Fixed2 {
    address public owner;

    error NotOwner();

    constructor() {
        owner = msg.sender;
    }

    function deposit() external payable {}

    // [FIX] msg.sender em vez de tx.origin — identifica o chamador direto.
    // Contratos intermedios nao conseguem passar esta verificacao.
    function withdraw(address payable to, uint256 amount) external {
        if (msg.sender != owner) revert NotOwner();
        require(amount <= address(this).balance, "insufficient balance");

        (bool ok, ) = to.call{value: amount}("");
        require(ok, "transfer failed");
    }
}
