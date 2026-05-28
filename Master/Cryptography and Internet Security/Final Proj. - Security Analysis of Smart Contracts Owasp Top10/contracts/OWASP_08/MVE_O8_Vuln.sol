// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O8_Vuln {
    mapping(address => uint256) public balanceOf;

    function deposit() external payable {
        balanceOf[msg.sender] += msg.value;
    }

    function withdraw(uint256 amount) external {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");

        // [VULN] Interacao externa antes de atualizar estado (reentrancy).
        (bool ok, ) = msg.sender.call{value: amount}("");
        require(ok, "transfer failed");

        balanceOf[msg.sender] -= amount;
    }
}
