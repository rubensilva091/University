// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O1_Vuln {
    address public owner;
    address payable public treasury;

    constructor() {
        owner = msg.sender;
        treasury = payable(msg.sender);
    }

    function deposit() external payable {}

    // [VULN] Sem controlo de acesso, qualquer endereco pode trocar a treasury.
    function setTreasury(address payable newTreasury) external {
        treasury = newTreasury;
    }

    // [VULN] Qualquer utilizador pode disparar o levantamento total para a treasury manipulada.
    function emergencyWithdraw() external {
        (bool ok, ) = treasury.call{value: address(this).balance}("");
        // [REVERT] "transfer failed" se a transferencia externa falhar.
        require(ok, "transfer failed");
    }
}
