// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O9_Vuln {
    string public constant name = "Overflow Token";
    string public constant symbol = "OVF";
    uint8 public constant decimals = 18;

    uint256 public constant MAX_SUPPLY = 1_000_000 ether;
    uint256 public totalSupply;
    mapping(address => uint256) public balanceOf;

    function mint(uint256 amount) external {
        // [VULN] unchecked permite wrap/overflow e pode contornar validacao de cap.
        unchecked {
            totalSupply += amount;
        }
        // [REVERT] "cap exceeded" apenas apos o wrap, logo a verificacao pode ser enganada.
        require(totalSupply <= MAX_SUPPLY, "cap exceeded");

        balanceOf[msg.sender] += amount;
    }
}
