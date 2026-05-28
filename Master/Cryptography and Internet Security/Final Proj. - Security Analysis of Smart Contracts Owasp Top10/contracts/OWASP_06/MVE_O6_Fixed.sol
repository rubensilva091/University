// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O6_Fixed {
    mapping(address => uint256) public credited;

    function fundPool() external payable {}

    function schedulePayment(address to, uint256 amount) external {
        require(amount <= address(this).balance, "insufficient pool");

        (bool ok, ) = to.call{value: amount}("");
        // [FIX] So avanca se chamada externa realmente tiver sucesso.
        require(ok, "payment failed");
        credited[to] += amount;
    }

    function rebate() external {
        uint256 amount = credited[msg.sender];
        require(amount > 0, "nothing credited");
        credited[msg.sender] = 0;

        (bool ok, ) = msg.sender.call{value: amount}("");
        // [REVERT] "rebate failed" se rebate falhar.
        require(ok, "rebate failed");
    }
}
