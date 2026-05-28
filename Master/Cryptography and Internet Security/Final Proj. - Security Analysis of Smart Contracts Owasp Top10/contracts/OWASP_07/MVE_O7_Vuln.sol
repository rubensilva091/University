// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O7_Vuln {
    mapping(address => uint256) public shares;
    uint256 public totalShares;

    function deposit() external payable {
        uint256 minted;
        uint256 totalAssetsBefore = address(this).balance - msg.value;

        if (totalShares == 0) {
            minted = msg.value;
        } else {
            minted = (msg.value * totalShares) / totalAssetsBefore;
        }

        shares[msg.sender] += minted;
        totalShares += minted;
    }

    function withdraw(uint256 assets) external {
        // [REVERT] "insufficient vault assets" se o cofre nao tiver saldo.
        require(assets <= address(this).balance, "insufficient vault assets");

        // [VULN] Divisao antes da multiplicacao pode dar burn == 0 em levantamentos pequenos.
        uint256 burn = (assets / address(this).balance) * totalShares;
        // [REVERT] "insufficient shares" so dispara se burn calculado ultrapassar shares.
        require(shares[msg.sender] >= burn, "insufficient shares");

        shares[msg.sender] -= burn;
        totalShares -= burn;

        (bool ok, ) = msg.sender.call{value: assets}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
