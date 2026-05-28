// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_09/MVE_O9_Vuln.sol";
import "../contracts/OWASP_09/MVE_O9_Fixed.sol";

// OWASP-09: Integer Overflow — Verificação Formal com Halmos
//
// Porquê dois endereços são necessários:
//   O overflow exige dois mints de endereços DIFERENTES.
//   Mint 1 (check contract): totalSupply = MAX_SUPPLY, balanceOf[check] = MAX_SUPPLY
//   Mint 2 (MintHelper):     unchecked → totalSupply wraps para 0, require(0 ≤ MAX_SUPPLY) passa,
//                             balanceOf[helper] = 2^256 - MAX_SUPPLY  (sem overflow — era 0)
//   Se ambos os mints viessem do mesmo endereço, o += em balanceOf reverteria
//   no segundo mint (Solidity 0.8 checked arithmetic), nunca chegando ao assert.
//
// Invariante testada: totalSupply nunca deve DECRECER após um mint bem-sucedido.
//   No Vuln: totalSupply vai de MAX_SUPPLY → 0 (decresce) → assert FALHA → COUNTEREXAMPLE.
//   No Fixed: qualquer mint que excedesse o cap é rejeitado → totalSupply monotónico → PASS.
//
// Resultado esperado:
//   VulnCheck  → FAIL  (Halmos encontra a1=MAX_SUPPLY, a2=2^256-MAX_SUPPLY)
//   FixedCheck → PASS  (Halmos prova monotonia para todos os inputs)

// ── Helper: segundo endereço para o segundo mint ──────────────────────────────

contract O9MintHelper {
    MVE_O9_Vuln public target;

    constructor(MVE_O9_Vuln _target) {
        target = _target;
    }

    function mint(uint256 amount) external {
        try target.mint(amount) {} catch {}
    }
}

contract O9MintHelperFixed {
    MVE_O9_Fixed public target;

    constructor(MVE_O9_Fixed _target) {
        target = _target;
    }

    function mint(uint256 amount) external {
        try target.mint(amount) {} catch {}
    }
}

// ── Vuln: unchecked permite overflow → totalSupply decresce ──────────────────

contract OWASP_09_VulnCheck {
    MVE_O9_Vuln target;
    O9MintHelper helper;

    constructor() {
        target = new MVE_O9_Vuln();
        helper = new O9MintHelper(target);
    }

    // Halmos explora simbolicamente a1 e a2.
    // Contraexemplo: a1 = MAX_SUPPLY, a2 = 2^256 - MAX_SUPPLY
    //   → totalSupply: 0 → MAX_SUPPLY → 0 (wrap) → assert(0 >= MAX_SUPPLY) FALHA
    function check_supply_monotonic(uint256 a1, uint256 a2) public {
        try target.mint(a1) {} catch {}     // endereço: check contract
        uint256 mid = target.totalSupply();
        helper.mint(a2);                    // endereço: helper (≠ check contract)
        assert(target.totalSupply() >= mid);
    }
}

// ── Fixed: require antes do mint → cap nunca excedido, totalSupply monotónico ─

contract OWASP_09_FixedCheck {
    MVE_O9_Fixed target;
    O9MintHelperFixed helper;

    constructor() {
        target = new MVE_O9_Fixed();
        helper = new O9MintHelperFixed(target);
    }

    // No Fixed: require(totalSupply + amount <= MAX_SUPPLY) rejeita qualquer mint
    // que causaria overflow → totalSupply nunca decresce → assert mantém-se → PASS.
    function check_supply_monotonic(uint256 a1, uint256 a2) public {
        try target.mint(a1) {} catch {}
        uint256 mid = target.totalSupply();
        helper.mint(a2);
        assert(target.totalSupply() >= mid);
    }
}
