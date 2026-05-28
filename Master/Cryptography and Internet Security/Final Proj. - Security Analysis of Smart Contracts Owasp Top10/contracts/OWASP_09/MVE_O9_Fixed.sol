// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O9_Fixed {
    string public constant name = "Safe Cap Token";
    string public constant symbol = "SCAP";
    uint8 public constant decimals = 18;

    uint256 public constant MAX_SUPPLY = 1_000_000 ether;
    uint256 public totalSupply;
    mapping(address => uint256) public balanceOf;

    function mint(uint256 amount) external {
        // [FIX] Valida amount minimo para evitar mint nulo/invalido.
        require(amount > 0, "invalid amount");
        // [FIX] Verifica cap antes de atualizar estado (sem unchecked).
        require(totalSupply + amount <= MAX_SUPPLY, "cap exceeded");

        totalSupply += amount;
        balanceOf[msg.sender] += amount;
    }
}
