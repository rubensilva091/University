// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O8_Fixed {
    mapping(address => uint256) public balanceOf;
    bool private locked;

    modifier nonReentrant() {
        require(!locked, "reentrant call");
        locked = true;
        _;
        locked = false;
    }

    function deposit() external payable {
        balanceOf[msg.sender] += msg.value;
    }

    function withdraw(uint256 amount) external nonReentrant {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");

        // [FIX] CEI: atualiza estado antes da chamada externa.
        balanceOf[msg.sender] -= amount;

        (bool ok, ) = msg.sender.call{value: amount}("");
        require(ok, "transfer failed");
    }
}
