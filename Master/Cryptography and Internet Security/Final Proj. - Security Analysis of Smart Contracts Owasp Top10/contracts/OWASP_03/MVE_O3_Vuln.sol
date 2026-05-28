// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MockSpotPool {
    uint256 public spotPriceE18; // Example: 2e18 means 1 collateral = 2 ETH of borrow power.

    constructor(uint256 initialPriceE18) {
        spotPriceE18 = initialPriceE18;
    }

    // [VULN] O preco pode ser manipulado instantaneamente.
    function setSpotPrice(uint256 newPriceE18) external {
        spotPriceE18 = newPriceE18;
    }
}

contract MVE_O3_Vuln {
    MockSpotPool public immutable pool;
    mapping(address => uint256) public collateral;

    constructor(address poolAddress) {
        pool = MockSpotPool(poolAddress);
    }

    function seedLiquidity() external payable {}

    function depositCollateral() external payable {
        collateral[msg.sender] += msg.value;
    }

    function maxBorrow(address user) public view returns (uint256) {
        uint256 spot = pool.spotPriceE18();
        // [VULN] Usa spot price manipulavel diretamente (sem TWAP).
        return (collateral[user] * spot * 50) / (100 * 1e18);
    }

    function borrow(uint256 amount) external {
        // [REVERT] "insufficient collateral" se o valor pedido passar o limite.
        require(amount <= maxBorrow(msg.sender), "insufficient collateral");
        // [REVERT] "insufficient liquidity" se o cofre nao tiver saldo.
        require(amount <= address(this).balance, "insufficient liquidity");

        (bool ok, ) = msg.sender.call{value: amount}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
