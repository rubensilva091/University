// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_09/MVE_O9_Vuln.sol";
import "../contracts/OWASP_09/MVE_O9_Fixed.sol";
import "../contracts/OWASP_09/MVE_O9_Vuln2.sol";
import "../contracts/OWASP_09/MVE_O9_Fixed2.sol";

// OWASP-09: Arithmetic Overflow — totalSupply must never exceed MAX_SUPPLY.
// Vuln: unchecked block wraps totalSupply around; with multiple senders (allContracts:true)
// sender A mints MAX_SUPPLY, sender B mints (2^256 - MAX_SUPPLY) → totalSupply wraps to 0
// while combined balances are astronomically large. Supply-cap invariant passes vacuously
// (totalSupply=0 ≤ MAX_SUPPLY), so we add a monotonicity invariant to catch the wrap.

contract OWASP_09_VulnFuzz {
    MVE_O9_Vuln public target;
    uint256 public snapshotSupply;

    constructor() payable {
        target = new MVE_O9_Vuln();
        snapshotSupply = 0;
    }

    // Wrapper: echidna calls this; records supply before mint so we can detect wrap-around.
    function mintAndSnapshot(uint256 amount) external {
        snapshotSupply = target.totalSupply();
        try target.mint(amount) {} catch {}
    }

    // Invariant 1: cap must never be exceeded.
    function echidna_supply_cap() external view returns (bool) {
        return target.totalSupply() <= target.MAX_SUPPLY();
    }

    // Invariant 2: supply must never DECREASE (wrap-around detection).
    // Vuln: unchecked overflow causes totalSupply to wrap from ~MAX to 0 → breaks this.
    function echidna_supply_never_decreases() external view returns (bool) {
        return target.totalSupply() >= snapshotSupply;
    }
}

contract OWASP_09_FixedFuzz {
    MVE_O9_Fixed public target;
    uint256 public snapshotSupply;

    constructor() payable {
        target = new MVE_O9_Fixed();
        snapshotSupply = 0;
    }

    function mintAndSnapshot(uint256 amount) external {
        snapshotSupply = target.totalSupply();
        try target.mint(amount) {} catch {}
    }

    function echidna_supply_cap() external view returns (bool) {
        return target.totalSupply() <= target.MAX_SUPPLY();
    }

    function echidna_supply_never_decreases() external view returns (bool) {
        return target.totalSupply() >= snapshotSupply;
    }
}

// OWASP-09 variante 2: underflow em burn() com bloco unchecked
// Vuln: burn(amount > balanceOf[caller]) → unchecked wrap para ~2^256.
//       Se totalSupply >= amount (outros utilizadores mintaram), a transacção completa.
// Fixed: require(balanceOf >= amount) impede o underflow.
// Deve ser DETECTADO por echidna/medusa (com allContracts:true).

contract OWASP_09_Vuln2Fuzz {
    MVE_O9_Vuln2 public target;

    constructor() payable {
        target = new MVE_O9_Vuln2();
    }

    function mintTokens(uint256 amount) external {
        uint256 v = 1 + (amount % 1e24);
        try target.mint(v) {} catch {}
    }

    function burnTokens(uint256 amount) external {
        // Pass arbitrary amounts — fuzzer will try values > balanceOf[this].
        try target.burn(amount) {} catch {}
    }

    // Invariant: balanceOf[this] must never exceed totalSupply.
    // Underflow wraps balanceOf[this] to ~2^256 while totalSupply stays small → BREAKS.
    function echidna_balance_le_supply() external view returns (bool) {
        return target.balanceOf(address(this)) <= target.totalSupply();
    }

    // Invariant: individual balance must stay within a sane bound (no wrap-around).
    function echidna_no_underflow_wrap() external view returns (bool) {
        return target.balanceOf(address(this)) <= 1e30;
    }
}

contract OWASP_09_Fixed2Fuzz {
    MVE_O9_Fixed2 public target;

    constructor() payable {
        target = new MVE_O9_Fixed2();
    }

    function mintTokens(uint256 amount) external {
        uint256 v = 1 + (amount % 1e24);
        try target.mint(v) {} catch {}
    }

    function burnTokens(uint256 amount) external {
        try target.burn(amount) {} catch {}
    }

    function echidna_balance_le_supply() external view returns (bool) {
        return target.balanceOf(address(this)) <= target.totalSupply();
    }

    function echidna_no_underflow_wrap() external view returns (bool) {
        return target.balanceOf(address(this)) <= 1e30;
    }
}
