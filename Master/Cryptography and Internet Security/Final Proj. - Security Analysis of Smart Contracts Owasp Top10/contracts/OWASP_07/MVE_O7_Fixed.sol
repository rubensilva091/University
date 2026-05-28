// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O7_Fixed {
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
        uint256 totalAssets = address(this).balance;
        // [REVERT] "insufficient vault assets" se o cofre nao tiver saldo.
        require(assets <= totalAssets, "insufficient vault assets");

        // [FIX] Multiplica antes de dividir e usa arredondamento para cima (ceil).
        uint256 burn = (assets * totalShares + totalAssets - 1) / totalAssets;
        // [REVERT] "insufficient shares" se o utilizador nao tiver shares suficientes.
        require(shares[msg.sender] >= burn, "insufficient shares");

        shares[msg.sender] -= burn;
        totalShares -= burn;

        (bool ok, ) = msg.sender.call{value: assets}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
