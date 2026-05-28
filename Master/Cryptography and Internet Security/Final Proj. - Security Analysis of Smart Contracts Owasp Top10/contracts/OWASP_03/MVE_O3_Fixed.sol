// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O3_Fixed {
    address public owner;
    uint256 public twapPriceE18;
    uint256 public lastPriceUpdate;
    mapping(address => uint256) public collateral;

    error NotOwner();
    error StaleOracle();

    modifier onlyOwner() {
        // [FIX] Protege atualizacao do preco contra atores nao autorizados.
        if (msg.sender != owner) revert NotOwner();
        _;
    }

    constructor(uint256 initialTwapPriceE18) {
        owner = msg.sender;
        twapPriceE18 = initialTwapPriceE18;
        lastPriceUpdate = block.timestamp;
    }

    function seedLiquidity() external payable {}

    function updateTwap(uint256 newTwapPriceE18) external onlyOwner {
        twapPriceE18 = newTwapPriceE18;
        lastPriceUpdate = block.timestamp;
    }

    function depositCollateral() external payable {
        collateral[msg.sender] += msg.value;
    }

    function maxBorrow(address user) public view returns (uint256) {
        // [FIX] Rejeita oracle stale para evitar uso de dados antigos.
        if (block.timestamp > lastPriceUpdate + 1 hours) revert StaleOracle();
        // [FIX] Usa TWAP em vez de spot manipulavel intra-bloco.
        return (collateral[user] * twapPriceE18 * 50) / (100 * 1e18);
    }

    function borrow(uint256 amount) external {
        // [REVERT] "insufficient collateral" se exceder maxBorrow.
        require(amount <= maxBorrow(msg.sender), "insufficient collateral");
        // [REVERT] "insufficient liquidity" se nao houver liquidez.
        require(amount <= address(this).balance, "insufficient liquidity");

        (bool ok, ) = msg.sender.call{value: amount}("");
        // [REVERT] "transfer failed" se a transferencia falhar.
        require(ok, "transfer failed");
    }
}
