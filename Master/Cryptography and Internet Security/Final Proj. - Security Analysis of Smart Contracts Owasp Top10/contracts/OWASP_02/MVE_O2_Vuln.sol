// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O2_Vuln {
    mapping(address => uint256) public deposits;
    mapping(address => uint256) public bonusOf;

    function deposit() external payable {
        deposits[msg.sender] += msg.value;
    }

    function claimBonus() external {
        require(deposits[msg.sender] >= 1 ether, "not eligible");
        // [VULN] Regra de negocio errada: bonus pode ser reclamado infinitas vezes.
        bonusOf[msg.sender] += 0.1 ether;
    }

    function withdrawAll() external {
        uint256 amount = deposits[msg.sender] + bonusOf[msg.sender];
        require(amount > 0, "nothing to withdraw");

        deposits[msg.sender] = 0;
        bonusOf[msg.sender] = 0;

        (bool ok, ) = msg.sender.call{value: amount}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
