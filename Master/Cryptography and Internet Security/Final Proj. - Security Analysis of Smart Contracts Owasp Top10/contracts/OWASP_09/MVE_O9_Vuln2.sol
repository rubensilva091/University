// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [VULN] OWASP-09 variante 2: Underflow com unchecked
// Solidity 0.8+ protege subtracao por defeito (reverte em underflow).
// O bloco unchecked desativa essa protecao: se amount > balanceOf[msg.sender],
// o resultado faz wrap para um valor enorme (~2^256 - diff), inflacionando o saldo.
contract MVE_O9_Vuln2 {
    string public constant name = "Underflow Token";
    string public constant symbol = "UDF";
    uint8 public constant decimals = 18;

    uint256 public totalSupply;
    mapping(address => uint256) public balanceOf;

    function mint(uint256 amount) external {
        require(amount > 0, "invalid amount");
        totalSupply += amount;
        balanceOf[msg.sender] += amount;
    }

    function burn(uint256 amount) external {
        // [VULN] unchecked permite underflow: amount > balanceOf[msg.sender]
        // produz wrap para ~2^256, inflacionando o saldo em vez de reverter.
        unchecked {
            balanceOf[msg.sender] -= amount;
        }
        totalSupply -= amount;
    }
}
