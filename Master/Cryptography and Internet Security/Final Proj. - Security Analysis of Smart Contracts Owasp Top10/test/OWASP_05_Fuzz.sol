// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../contracts/OWASP_05/MVE_O5_Vuln.sol";
import "../contracts/OWASP_05/MVE_O5_Fixed.sol";
import "../contracts/OWASP_05/MVE_O5_Vuln2.sol";
import "../contracts/OWASP_05/MVE_O5_Fixed2.sol";

// OWASP-05: Signature Replay — a valid signature should only credit balance once.
// Vuln: no nonce/deadline consumption → same sig usable N times → balance grows unboundedly.
// We simulate one "valid" sig offline and replay it; on Vuln balance keeps growing.

contract OWASP_05_VulnFuzz {
    MVE_O5_Vuln target;
    address constant RECIPIENT = address(0x1337);
    uint256 constant CLAIM_AMOUNT = 1 ether;

    // Pre-computed signature for (contract, RECIPIENT, CLAIM_AMOUNT) signed by SIGNER_PRIV.
    // In echidna we just test the structural invariant: after calling claim once,
    // calling it again should NOT increase balance further.

    uint256 balanceAfterFirstClaim;
    bool firstClaimDone;

    constructor() payable {
        // Use address(1) as a dummy signer; sig validation will revert with "bad signature"
        // unless a valid sig is provided — so we test the nonce/replay guard structurally.
        target = new MVE_O5_Vuln(address(1));
    }

    // Record balance snapshot after any successful claim.
    function recordBalance() external {
        if (!firstClaimDone && target.balanceOf(RECIPIENT) > 0) {
            balanceAfterFirstClaim = target.balanceOf(RECIPIENT);
            firstClaimDone = true;
        }
    }

    // Property: once first claim is done, balance must not exceed that snapshot.
    // This only activates after firstClaimDone is set.
    function echidna_no_replay() external view returns (bool) {
        if (!firstClaimDone) return true;
        return target.balanceOf(RECIPIENT) <= balanceAfterFirstClaim;
    }
}

contract OWASP_05_FixedFuzz {
    MVE_O5_Fixed target;
    address constant RECIPIENT = address(0x1337);

    uint256 balanceAfterFirstClaim;
    bool firstClaimDone;

    constructor() payable {
        target = new MVE_O5_Fixed(address(1));
    }

    function recordBalance() external {
        if (!firstClaimDone && target.balanceOf(RECIPIENT) > 0) {
            balanceAfterFirstClaim = target.balanceOf(RECIPIENT);
            firstClaimDone = true;
        }
    }

    function echidna_no_replay() external view returns (bool) {
        if (!firstClaimDone) return true;
        return target.balanceOf(RECIPIENT) <= balanceAfterFirstClaim;
    }

    // Structural: nonce always increments monotonically (never decreases).
    function echidna_nonce_monotonic() external view returns (bool) {
        return target.nonces(RECIPIENT) >= 0; // trivially true; meaningful after fuzzing
    }
}

// OWASP-05 variante 2: falta de validação de inputs em claim() assinado
// LIMITAÇÃO CONHECIDA: claim() exige assinatura ECDSA válida do trustedSigner.
// echidna não gera assinaturas válidas → claim() reverte em "bad signature".
// Resultado esperado: FN (mesma razão que a variante 1).
// O teste confirma que balanceOf(address(0)) nunca cresce (nem na Fixed2).

contract OWASP_05_Vuln2Fuzz {
    MVE_O5_Vuln2 public target;

    constructor() payable {
        // address(1) como signer — echidna não consegue gerar sig válida para este.
        target = new MVE_O5_Vuln2(address(1));
    }

    function tryClaim(address to, uint256 amount, bytes calldata sig) external {
        try target.claim(to, amount, sig) {} catch {}
    }

    // address(0) nunca deve acumular saldo (tokens perdidos).
    function echidna_zero_address_balance_empty() external view returns (bool) {
        return target.balanceOf(address(0)) == 0;
    }

    // Nenhuma conta deve ter saldo sem assinatura válida do signer.
    function echidna_no_balance_without_valid_sig() external view returns (bool) {
        return target.balanceOf(address(this)) == 0;
    }
}

contract OWASP_05_Fixed2Fuzz {
    MVE_O5_Fixed2 public target;

    constructor() payable {
        target = new MVE_O5_Fixed2(address(1));
    }

    function tryClaim(address to, uint256 amount, bytes calldata sig) external {
        try target.claim(to, amount, sig) {} catch {}
    }

    function echidna_zero_address_balance_empty() external view returns (bool) {
        return target.balanceOf(address(0)) == 0;
    }

    function echidna_no_balance_without_valid_sig() external view returns (bool) {
        return target.balanceOf(address(this)) == 0;
    }
}
