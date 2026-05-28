// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_01/MVE_O1_Vuln.sol";
import "../contracts/OWASP_01/MVE_O1_Fixed.sol";
import "../contracts/OWASP_01/MVE_O1_Vuln2.sol";
import "../contracts/OWASP_01/MVE_O1_Fixed2.sol";

// OWASP-01: Access Control — treasury must only be changeable by the owner.
// Strategy: allContracts:true lets echidna call target.setTreasury() directly
// from non-owner sender addresses. On Vuln there is no guard → treasury changes.
// On Fixed the onlyOwner modifier reverts non-owner calls → treasury is unchanged.

contract OWASP_01_VulnFuzz {
    MVE_O1_Vuln public target;
    address payable public immutable INITIAL_TREASURY;

    constructor() payable {
        target = new MVE_O1_Vuln();
        target.deposit{value: 1 ether}();
        INITIAL_TREASURY = payable(address(this));
    }

    receive() external payable {}

    // With allContracts:true, echidna calls target.setTreasury(random) from
    // non-owner addresses. Vuln: succeeds → treasury != INITIAL_TREASURY → breaks.
    function echidna_treasury_unchanged() external view returns (bool) {
        return address(target.treasury()) == address(INITIAL_TREASURY);
    }
}

contract OWASP_01_FixedFuzz {
    MVE_O1_Fixed public target;
    address payable public immutable INITIAL_TREASURY;

    constructor() payable {
        target = new MVE_O1_Fixed();
        target.deposit{value: 1 ether}();
        INITIAL_TREASURY = payable(address(this));
    }

    receive() external payable {}

    function echidna_treasury_unchanged() external view returns (bool) {
        return address(target.treasury()) == address(INITIAL_TREASURY);
    }
}

// OWASP-01 variante 2: tx.origin phishing
// LIMITAÇÃO CONHECIDA: echidna só faz chamadas directas (tx.origin == msg.sender).
// O ataque de phishing exige um contrato intermediário como chamador, que echidna
// não consegue simular. Resultado esperado: FN em ambos Vuln2 e Fixed2.
// O teste confirma o comportamento estrutural normal (só o owner pode retirar).

contract OWASP_01_Vuln2Fuzz {
    MVE_O1_Vuln2 public target;
    uint256 public ownerDeposited;   // tracks what THIS contract deposited
    uint256 public ownerWithdrawn;   // tracks what THIS contract withdrew

    constructor() payable {
        target = new MVE_O1_Vuln2(); // this contract IS the owner
        target.deposit{value: 2 ether}();
        ownerDeposited = 2 ether;
    }

    receive() external payable {
        ownerWithdrawn += msg.value;
    }

    function tryWithdraw(uint256 amount) external {
        uint256 v = amount % (ownerDeposited - ownerWithdrawn + 1);
        try target.withdraw(payable(address(this)), v) {} catch {}
    }

    // Saldo do contrato deve nunca exceder o total depositado (sem inflação).
    // Com allContracts:true echidna pode depositar diretamente; rastreamos só o
    // que o owner (este contrato) depositou vs retirou.
    // tx.origin FN esperado: echidna não consegue simular phishing de intermediário.
    function echidna_no_inflation() external view returns (bool) {
        return ownerWithdrawn <= ownerDeposited;
    }
}

contract OWASP_01_Fixed2Fuzz {
    MVE_O1_Fixed2 public target;
    uint256 public ownerDeposited;
    uint256 public ownerWithdrawn;

    constructor() payable {
        target = new MVE_O1_Fixed2();
        target.deposit{value: 2 ether}();
        ownerDeposited = 2 ether;
    }

    receive() external payable {
        ownerWithdrawn += msg.value;
    }

    function tryWithdraw(uint256 amount) external {
        uint256 v = amount % (ownerDeposited - ownerWithdrawn + 1);
        try target.withdraw(payable(address(this)), v) {} catch {}
    }

    function echidna_no_inflation() external view returns (bool) {
        return ownerWithdrawn <= ownerDeposited;
    }
}
