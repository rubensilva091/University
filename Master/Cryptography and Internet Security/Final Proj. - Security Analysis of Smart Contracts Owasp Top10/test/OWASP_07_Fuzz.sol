// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_07/MVE_O7_Vuln.sol";
import "../contracts/OWASP_07/MVE_O7_Fixed.sol";

// OWASP-07: Rounding / Precision — withdraw must always burn a proportional amount of shares.
// Vuln: (assets / balance) * shares — integer division first → burn == 0 for small withdrawals
//       → user receives ETH for free, shares never decrease → assets extracted without share cost.

contract OWASP_07_VulnFuzz {
    MVE_O7_Vuln target;
    uint256 initialShares;

    constructor() payable {
        target = new MVE_O7_Vuln();
        target.deposit{value: 10 ether}();
        initialShares = target.shares(address(this));
    }

    function withdraw(uint256 assets) external {
        try target.withdraw(assets) {} catch {}
    }

    // Property: after any withdrawal, shares must have decreased by at least 1.
    // If shares stay the same but ETH was received → rounding exploit → property breaks.
    function echidna_shares_decrease_on_withdraw() external view returns (bool) {
        uint256 currentShares = target.shares(address(this));
        uint256 currentBalance = address(target).balance;
        // If balance decreased (ETH left), shares must also have decreased.
        if (currentBalance < 10 ether) {
            return currentShares < initialShares;
        }
        return true;
    }
}

contract OWASP_07_FixedFuzz {
    MVE_O7_Fixed target;
    uint256 initialShares;

    constructor() payable {
        target = new MVE_O7_Fixed();
        target.deposit{value: 10 ether}();
        initialShares = target.shares(address(this));
    }

    function withdraw(uint256 assets) external {
        try target.withdraw(assets) {} catch {}
    }

    function echidna_shares_decrease_on_withdraw() external view returns (bool) {
        uint256 currentShares = target.shares(address(this));
        uint256 currentBalance = address(target).balance;
        if (currentBalance < 10 ether) {
            return currentShares < initialShares;
        }
        return true;
    }
}
