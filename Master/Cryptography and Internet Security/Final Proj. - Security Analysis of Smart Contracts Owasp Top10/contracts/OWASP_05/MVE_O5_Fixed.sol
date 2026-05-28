// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O5_Fixed {
    address public immutable trustedSigner;
    mapping(address => uint256) public balanceOf;
    mapping(address => uint256) public nonces;

    constructor(address signer) {
        trustedSigner = signer;
    }

    function claim(
        address to,
        uint256 amount,
        uint256 deadline,
        uint256 nonce,
        bytes calldata sig
    ) external {
        // [FIX] Valida recipient.
        // [REVERT] "invalid recipient" se endereco for zero.
        require(to != address(0), "invalid recipient");
        // [FIX] Valida expiracao temporal.
        // [REVERT] "signature expired" se a assinatura ja expirou.
        require(block.timestamp <= deadline, "signature expired");
        // [FIX] Impede replay exigindo nonce exato.
        // [REVERT] "invalid nonce" se nonce nao corresponder ao esperado.
        require(nonce == nonces[to], "invalid nonce");

        bytes32 digest = keccak256(
            abi.encodePacked(address(this), block.chainid, to, amount, deadline, nonce)
        );
        address recovered = _recover(digest, sig);
        // [REVERT] "bad signature" se a assinatura nao for valida.
        require(recovered == trustedSigner, "bad signature");

        // [FIX] Consome nonce para invalidar reutilizacao da assinatura.
        nonces[to] += 1;
        balanceOf[to] += amount;
    }

    function _recover(bytes32 digest, bytes calldata signature) internal pure returns (address) {
        require(signature.length == 65, "invalid signature length");

        bytes32 r;
        bytes32 s;
        uint8 v;

        assembly {
            r := calldataload(signature.offset)
            s := calldataload(add(signature.offset, 32))
            v := byte(0, calldataload(add(signature.offset, 64)))
        }

        return ecrecover(digest, v, r, s);
    }
}
