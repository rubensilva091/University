// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_10/MVE_O10_Vuln.sol";
import "../contracts/OWASP_10/MVE_O10_Fixed.sol";

// OWASP-10: Uninitialized Proxy — owner must be set at construction and never claimable by outsider.
// Vuln: initialize() is public and callable by anyone before deployer acts → ownership takeover.

contract OWASP_10_VulnFuzz {
    MVE_O10_Vuln target;
    address constant ATTACKER = address(0x41);
    address constant IMPL = address(0x1337);

    constructor() payable {
        target = new MVE_O10_Vuln();
        // Deployer does NOT call initialize() — simulates the uninitialized window.
    }

    function tryInitialize(address newOwner, address impl) external {
        try target.initialize(newOwner, impl) {} catch {}
    }

    // Property: owner should never be the ATTACKER address.
    function echidna_owner_not_attacker() external view returns (bool) {
        return target.owner() != ATTACKER;
    }

    // Property: owner should be address(this) (deployer) or address(0).
    function echidna_owner_is_deployer() external view returns (bool) {
        address o = target.owner();
        return o == address(this) || o == address(0);
    }
}

contract OWASP_10_FixedFuzz {
    MVE_O10_Fixed target;
    address constant ATTACKER = address(0x41);
    address constant IMPL = address(0x1337);

    constructor() payable {
        target = new MVE_O10_Fixed(IMPL);
        // owner is set in constructor — no window.
    }

    // Property: owner is always the deployer (address(this)).
    function echidna_owner_is_deployer() external view returns (bool) {
        return target.owner() == address(this);
    }

    // Property: a non-owner upgradeTo call should never succeed.
    function echidna_impl_controlled() external view returns (bool) {
        return target.implementation() == IMPL;
    }
}
