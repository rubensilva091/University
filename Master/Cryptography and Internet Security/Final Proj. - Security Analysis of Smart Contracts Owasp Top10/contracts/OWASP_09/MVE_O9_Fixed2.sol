// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [FIX] OWASP-09 variante 2: Underflow prevenido
// Duas defesas:
//   1. require() explicito garante saldo suficiente antes da subtracao.
//   2. Sem unchecked — o check aritmetico do Solidity 0.8 actua como segunda linha de defesa.
contract MVE_O9_Fixed2 {
    string public constant name = "Safe Burn Token";
    string public constant symbol = "SBURN";
    uint8 public constant decimals = 18;

    uint256 public totalSupply;
    mapping(address => uint256) public balanceOf;

    function mint(uint256 amount) external {
        require(amount > 0, "invalid amount");
        totalSupply += amount;
        balanceOf[msg.sender] += amount;
    }

    function burn(uint256 amount) external {
        // [FIX] Validacao explicita antes da subtracao.
        require(amount > 0, "invalid amount");
        require(balanceOf[msg.sender] >= amount, "insufficient balance");

        // Sem unchecked — Solidity 0.8 reverte em underflow por defeito.
        balanceOf[msg.sender] -= amount;
        totalSupply -= amount;
    }
}
