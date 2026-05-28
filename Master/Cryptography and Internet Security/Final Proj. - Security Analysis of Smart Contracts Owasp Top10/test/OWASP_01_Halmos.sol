// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_01/MVE_O1_Vuln.sol";
import "../contracts/OWASP_01/MVE_O1_Fixed.sol";

// OWASP-01: Access Control — Verificação Formal com Halmos
//
// Estratégia: sem forge-std/vm.prank, usamos um contrato Attacker separado.
// O check contract faz deploy do alvo (torna-se owner) e do Attacker (endereço diferente).
// Quando Attacker chama setTreasury(), msg.sender = Attacker ≠ owner.
// Halmos explora simbolicamente todos os valores possíveis de newTreasury.
//
// Resultado esperado:
//   VulnCheck  → FAIL  (Halmos encontra counterexample: treasury muda)
//   FixedCheck → PASS  (Halmos prova: treasury nunca muda com chamada externa)

// ── Helper: endereço não-owner que tenta mudar a treasury ────────────────────

contract O1Attacker {
    MVE_O1_Vuln public target;

    constructor(MVE_O1_Vuln _target) {
        target = _target;
    }

    function attack(address payable newTreasury) external {
        target.setTreasury(newTreasury);
    }
}

contract O1AttackerFixed {
    MVE_O1_Fixed public target;

    constructor(MVE_O1_Fixed _target) {
        target = _target;
    }

    function attack(address payable newTreasury) external {
        target.setTreasury(newTreasury);
    }
}

// ── Vuln: sem onlyOwner → qualquer endereço altera a treasury ────────────────

contract OWASP_01_VulnCheck {
    MVE_O1_Vuln target;
    O1Attacker attacker;

    constructor() {
        target  = new MVE_O1_Vuln();          // este contrato é o owner
        attacker = new O1Attacker(target);    // endereço distinto → não é owner
    }

    // Halmos chama esta função com newTreasury simbólico.
    // No Vuln: setTreasury não tem guard → treasury muda → assert falha → COUNTEREXAMPLE.
    function check_non_owner_cannot_change_treasury(address payable newTreasury) public {
        address before = target.treasury();
        try attacker.attack(newTreasury) {} catch {}
        assert(target.treasury() == before);
    }
}

// ── Fixed: onlyOwner reverte → treasury mantém-se para qualquer input ────────

contract OWASP_01_FixedCheck {
    MVE_O1_Fixed target;
    O1AttackerFixed attacker;

    constructor() {
        target   = new MVE_O1_Fixed();
        attacker = new O1AttackerFixed(target);
    }

    // Halmos chama com newTreasury simbólico.
    // No Fixed: onlyOwner reverte a chamada do Attacker → treasury nunca muda → PASS.
    function check_non_owner_cannot_change_treasury(address payable newTreasury) public {
        address before = target.treasury();
        try attacker.attack(newTreasury) {} catch {}
        assert(target.treasury() == before);
    }
}
