// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_04/MVE_O4_Vuln.sol";
import "../contracts/OWASP_04/MVE_O4_Fixed.sol";

// OWASP-04: Flash Bonus Abuse — same-block deposit+withdraw must never succeed.
// Vuln: no cooldown → atomicFlashAttack succeeds → property breaks.
// Fixed: cooldown (block.number > lastDepositBlock) → withdrawAll reverts in same block.
// Property: atomicFlashAttack must NEVER complete withdrawAll successfully.

contract OWASP_04_VulnFuzz {
    MVE_O4_Vuln target;
    uint256 constant POOL_SEED = 10 ether;
    bool public atomicAttackSucceeded;

    constructor() payable {
        target = new MVE_O4_Vuln();
        target.fundPool{value: POOL_SEED}();
    }

    receive() external payable {}

    // Atomic deposit+withdraw in a single call (same block).
    // On Vuln: no cooldown → withdrawAll succeeds → atomicAttackSucceeded = true.
    // On Fixed: cooldown check fails → withdrawAll reverts → atomicAttackSucceeded stays false.
    function atomicFlashAttack() external {
        if (address(this).balance < 10 ether) return;
        target.deposit{value: 10 ether}();
        try target.withdrawAll() {
            atomicAttackSucceeded = true;
        } catch {}
    }

    // Property: the same-block attack must never succeed.
    function echidna_no_atomic_drain() external view returns (bool) {
        return !atomicAttackSucceeded;
    }
}

contract OWASP_04_FixedFuzz {
    MVE_O4_Fixed target;
    uint256 constant POOL_SEED = 10 ether;
    bool public atomicAttackSucceeded;

    constructor() payable {
        target = new MVE_O4_Fixed();
        target.fundPool{value: POOL_SEED}();
    }

    receive() external payable {}

    function atomicFlashAttack() external {
        if (address(this).balance < 10 ether) return;
        target.deposit{value: 10 ether}();
        try target.withdrawAll() {
            atomicAttackSucceeded = true;
        } catch {}
    }

    function echidna_no_atomic_drain() external view returns (bool) {
        return !atomicAttackSucceeded;
    }
}
