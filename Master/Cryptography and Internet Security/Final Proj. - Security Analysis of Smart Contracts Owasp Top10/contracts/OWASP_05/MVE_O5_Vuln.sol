// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MVE_O5_Vuln {
    address public immutable trustedSigner;
    mapping(address => uint256) public balanceOf;

    constructor(address signer) {
        trustedSigner = signer;
    }

    // [VULN] Falta validacao forte de input.
    // deadline ignorado + sem nonce + recipient potencialmente invalido => replay.
    function claim(
        address to,
        uint256 amount,
        uint256 deadline,
        bytes calldata sig
    ) external {
        bytes32 digest = keccak256(abi.encodePacked(address(this), to, amount));
        address recovered = _recover(digest, sig);
        // [REVERT] "bad signature" se a assinatura nao for do signer esperado.
        require(recovered == trustedSigner, "bad signature");

        deadline; // silences warning; still intentionally unused.
        // [VULN] Credita estado sem expirar/consumir assinatura (permite replay).
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
