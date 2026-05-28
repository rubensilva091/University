// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_06/MVE_O6_Vuln.sol";
import "../contracts/OWASP_06/MVE_O6_Fixed.sol";
import {MVE_O6_Vuln2, IERC20Minimal} from "../contracts/OWASP_06/MVE_O6_Vuln2.sol";
import {MVE_O6_Fixed2} from "../contracts/OWASP_06/MVE_O6_Fixed2.sol";

// OWASP-06: Unchecked Return Value — credited balance must only grow on successful transfers.
// Vuln: ignored return value means failed transfer still credits balance → double-spend via rebate.

contract OWASP_06_VulnFuzz {
    MVE_O6_Vuln target;
    uint256 constant POOL_SEED = 5 ether;
    // RevertingReceiver: any ETH transfer to it fails.
    RevertingReceiver receiver;

    constructor() payable {
        target = new MVE_O6_Vuln();
        target.fundPool{value: POOL_SEED}();
        receiver = new RevertingReceiver();
    }

    // Schedule payment to reverting receiver — on Vuln, credited still grows.
    function scheduleToReverter(uint256 amount) external {
        uint256 v = 1 + (amount % (address(target).balance));
        try target.schedulePayment(address(receiver), v) {} catch {}
    }

    // Property: credited[receiver] should be 0 because every transfer to it fails.
    function echidna_no_phantom_credit() external view returns (bool) {
        return target.credited(address(receiver)) == 0;
    }
}

contract OWASP_06_FixedFuzz {
    MVE_O6_Fixed target;
    uint256 constant POOL_SEED = 5 ether;
    RevertingReceiver receiver;

    constructor() payable {
        target = new MVE_O6_Fixed();
        target.fundPool{value: POOL_SEED}();
        receiver = new RevertingReceiver();
    }

    function scheduleToReverter(uint256 amount) external {
        uint256 v = 1 + (amount % (address(target).balance + 1));
        try target.schedulePayment(address(receiver), v) {} catch {}
    }

    function echidna_no_phantom_credit() external view returns (bool) {
        return target.credited(address(receiver)) == 0;
    }
}

// Helper: always reverts on ETH receive.
contract RevertingReceiver {
    receive() external payable {
        revert("rejected");
    }
}

// Helper: ERC20 que sempre retorna false (estilo USDT com saldo insuficiente).
contract ReturnsFalseToken {
    function transfer(address, uint256) external pure returns (bool) {
        return false;
    }
}

// OWASP-06 variante 2: ERC20 transfer() ignorado (retorna false sem reverter)
// Vuln: allocated[user] é zerado mesmo quando transfer() retorna false.
//       O utilizador perde a sua alocação sem receber os tokens.
// Fixed: claim() reverte ao detectar false → allocated mantém-se intacto.
// Deve ser DETECTADO por echidna/medusa.

contract OWASP_06_Vuln2Fuzz {
    MVE_O6_Vuln2 public target;
    ReturnsFalseToken public mockToken;
    uint256 public constant ALLOC = 1000 ether;

    constructor() payable {
        mockToken = new ReturnsFalseToken();
        target = new MVE_O6_Vuln2(address(mockToken));
        // This contract is the owner — allocate to itself.
        target.allocate(address(this), ALLOC);
    }

    function tryClaim() external {
        try target.claim() {} catch {}
    }

    // After claim() with a token that returns false:
    // Vuln: allocated[this] becomes 0 even though no tokens were received → BREAKS.
    // Fixed: claim() reverts → allocated[this] stays ALLOC → holds.
    function echidna_allocation_preserved_on_failed_transfer() external view returns (bool) {
        return target.allocated(address(this)) == ALLOC;
    }
}

contract OWASP_06_Fixed2Fuzz {
    MVE_O6_Fixed2 public target;
    ReturnsFalseToken public mockToken;
    uint256 public constant ALLOC = 1000 ether;

    constructor() payable {
        mockToken = new ReturnsFalseToken();
        target = new MVE_O6_Fixed2(address(mockToken));
        target.allocate(address(this), ALLOC);
    }

    function tryClaim() external {
        try target.claim() {} catch {}
    }

    function echidna_allocation_preserved_on_failed_transfer() external view returns (bool) {
        return target.allocated(address(this)) == ALLOC;
    }
}
