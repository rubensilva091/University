// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_03/MVE_O3_Vuln.sol";
import "../contracts/OWASP_03/MVE_O3_Fixed.sol";

// OWASP-03: Oracle Manipulation — borrow should never exceed safe_max at the initial price.
// Vuln: spot price freely settable → inflate price → over-borrow → property breaks.
// With allContracts:true echidna calls pool.setSpotPrice(huge); borrow wrapper uses fuzz collateral.

contract OWASP_03_VulnFuzz {
    MockSpotPool public pool;
    MVE_O3_Vuln public target;
    uint256 constant COLLATERAL = 1 ether;
    uint256 constant INITIAL_PRICE = 2e18;
    // Safe max at honest price: collateral * price * 50 / (100 * 1e18) = 1 ether
    uint256 constant SAFE_MAX_BORROW = 1 ether;
    // Track initial target balance to compute how much was borrowed.
    uint256 public initialBalance;

    constructor() payable {
        pool = new MockSpotPool(INITIAL_PRICE);
        target = new MVE_O3_Vuln(address(pool));
        target.seedLiquidity{value: 10 ether}();
        target.depositCollateral{value: COLLATERAL}();
        initialBalance = address(target).balance; // = 11 ether
    }

    receive() external payable {}

    // Wrapper: fuzz contract (which has collateral) borrows from the target.
    // With allContracts:true, echidna also calls pool.setSpotPrice() directly to manipulate.
    function borrow(uint256 amount) external {
        try target.borrow(amount) {} catch {}
    }

    // Property: total borrowed (initial_balance - current_balance) must never exceed SAFE_MAX.
    function echidna_no_overborrow() external view returns (bool) {
        uint256 current = address(target).balance;
        if (current >= initialBalance) return true; // nothing borrowed
        uint256 borrowed = initialBalance - current;
        return borrowed <= SAFE_MAX_BORROW;
    }
}

contract OWASP_03_FixedFuzz {
    MVE_O3_Fixed public target;
    uint256 constant COLLATERAL = 1 ether;
    uint256 constant INITIAL_PRICE = 2e18;
    uint256 constant SAFE_MAX_BORROW = 1 ether;
    uint256 public initialBalance;

    constructor() payable {
        target = new MVE_O3_Fixed(INITIAL_PRICE);
        target.seedLiquidity{value: 10 ether}();
        target.depositCollateral{value: COLLATERAL}();
        initialBalance = address(target).balance;
    }

    receive() external payable {}

    function borrow(uint256 amount) external {
        try target.borrow(amount) {} catch {}
    }

    function echidna_no_overborrow() external view returns (bool) {
        uint256 current = address(target).balance;
        if (current >= initialBalance) return true;
        uint256 borrowed = initialBalance - current;
        return borrowed <= SAFE_MAX_BORROW;
    }
}
