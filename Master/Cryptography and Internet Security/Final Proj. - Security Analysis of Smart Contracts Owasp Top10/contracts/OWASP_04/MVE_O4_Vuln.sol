// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O4_Vuln {
    mapping(address => uint256) public principal;
    mapping(address => uint256) public bonus;

    function fundPool() external payable {}

    function deposit() external payable {
        principal[msg.sender] += msg.value;
        if (msg.value >= 10 ether) {
            // [VULN] Bonus imediato permite abuso com capital temporario (flash-like).
            bonus[msg.sender] += 1 ether;
        }
    }

    function withdrawAll() external {
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
