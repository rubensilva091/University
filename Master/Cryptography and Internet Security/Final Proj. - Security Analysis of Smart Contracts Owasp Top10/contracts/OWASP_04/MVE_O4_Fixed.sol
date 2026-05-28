// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O4_Fixed {
    mapping(address => uint256) public principal;
    mapping(address => uint256) public bonus;
    mapping(address => uint256) public lastDepositBlock;

    function fundPool() external payable {}

    function deposit() external payable {
        principal[msg.sender] += msg.value;
        lastDepositBlock[msg.sender] = block.number;

        if (msg.value >= 10 ether) {
            bonus[msg.sender] += 1 ether;
        }
    }

    function withdrawAll() external {
        // [FIX] Impoe cooldown para bloquear operacao atomica de ataque.
        require(block.number > lastDepositBlock[msg.sender], "cooldown not met");

        uint256 amount = principal[msg.sender] + bonus[msg.sender];
        require(amount > 0, "nothing");
        require(amount <= address(this).balance, "insufficient pool");

        principal[msg.sender] = 0;
        bonus[msg.sender] = 0;

        (bool ok, ) = msg.sender.call{value: amount}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
