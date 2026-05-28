// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [VULN] OWASP-05 variante 2: Falta de validacao de inputs em operacao assinada
// O contrato usa uma assinatura para autorizar claims mas nao valida:
//   - Se o destinatario e um endereco valido (pode ser address(0))
//   - Se o montante esta dentro de limites razoaveis (pode ser 0 ou astronomico)
// Estas lacunas permitem claims absurdos que o signer provavelmente nao autorizou.
contract MVE_O5_Vuln2 {
    address public immutable trustedSigner;
    mapping(address => uint256) public balanceOf;
    mapping(bytes32 => bool) public usedSigs;

    constructor(address signer) {
        trustedSigner = signer;
    }

    function claim(
        address to,
        uint256 amount,
        bytes calldata sig
    ) external {
        // [VULN] Sem validacao de to: pode ser address(0) — tokens perdidos.
        // [VULN] Sem validacao de amount: pode ser 0 (inutil) ou MAX_UINT256 (overflow em balanceOf).
        bytes32 digest = keccak256(abi.encodePacked(address(this), to, amount));
        require(!usedSigs[digest], "already used");

        address recovered = _recover(digest, sig);
        require(recovered == trustedSigner, "bad signature");

        usedSigs[digest] = true;
        balanceOf[to] += amount;
    }

    function _recover(bytes32 digest, bytes calldata sig) internal pure returns (address) {
        require(sig.length == 65, "invalid signature length");
        bytes32 r;
        bytes32 s;
        uint8 v;
        assembly {
            r := calldataload(sig.offset)
            s := calldataload(add(sig.offset, 32))
            v := byte(0, calldataload(add(sig.offset, 64)))
        }
        return ecrecover(digest, v, r, s);
    }
}
