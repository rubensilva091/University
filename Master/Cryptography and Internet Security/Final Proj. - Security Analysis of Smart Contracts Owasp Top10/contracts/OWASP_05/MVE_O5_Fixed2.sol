// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [FIX] OWASP-05 variante 2: Validacao rigorosa de inputs em operacao assinada
// Todas as entradas sao validadas antes de processar a assinatura:
//   - to != address(0) — impede perda de tokens
//   - amount em [1, MAX_CLAIM] — impede valores absurdos ou overflow
contract MVE_O5_Fixed2 {
    address public immutable trustedSigner;
    uint256 public constant MAX_CLAIM = 10_000 ether;
    mapping(address => uint256) public balanceOf;
    mapping(bytes32 => bool) public usedSigs;

    error InvalidRecipient();
    error InvalidAmount();

    constructor(address signer) {
        trustedSigner = signer;
    }

    function claim(
        address to,
        uint256 amount,
        bytes calldata sig
    ) external {
        // [FIX] Valida destinatario — impede tokens enviados para address(0).
        if (to == address(0)) revert InvalidRecipient();
        // [FIX] Valida intervalo do montante — impede claims nulos ou excessivos.
        if (amount == 0 || amount > MAX_CLAIM) revert InvalidAmount();

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
