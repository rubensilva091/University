// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_02/MVE_O2_Vuln.sol";
import "../contracts/OWASP_02/MVE_O2_Fixed.sol";

// OWASP-02: Business Logic — bonus can only be claimed once per address.
// Vuln: claimBonus has no "already claimed" guard → bonus accumulates → property breaks.

contract OWASP_02_VulnFuzz {
    MVE_O2_Vuln target;
    uint256 constant SEED = 2 ether;
    uint256 immutable MAX_BONUS;

    constructor() payable {
        target = new MVE_O2_Vuln();
        target.deposit{value: SEED}();
        MAX_BONUS = 0.1 ether; // one legitimate claim
    }

    function claimBonus() external {
        target.claimBonus();
    }

    // bonusOf should never exceed one claim (0.1 ether) for a single depositor.
    function echidna_bonus_claimed_once() external returns (bool) {
        return target.bonusOf(address(this)) <= MAX_BONUS;
    }
}

contract OWASP_02_FixedFuzz {
    MVE_O2_Fixed target;
    uint256 constant MAX_BONUS = 0.1 ether;

    constructor() payable {
        target = new MVE_O2_Fixed();
        target.deposit{value: 2 ether}();
    }

    function claimBonus() external {
        target.claimBonus();
    }

    function echidna_bonus_claimed_once() external returns (bool) {
        return target.bonusOf(address(this)) <= MAX_BONUS;
    }
}
