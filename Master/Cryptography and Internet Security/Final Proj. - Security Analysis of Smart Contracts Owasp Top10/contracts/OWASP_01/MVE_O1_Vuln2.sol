// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [VULN] OWASP-01 variante 2: Autenticacao via tx.origin (phishing)
// tx.origin e sempre o EOA que iniciou a transacao, nao o chamador imediato.
// Um contrato malicioso pode induzir o owner a interagir com ele e, durante
// essa interacao, chamar withdraw() — tx.origin sera o owner mas msg.sender
// sera o contrato do atacante, contornando o controlo de acesso pretendido.
contract MVE_O1_Vuln2 {
    address public owner;

    constructor() {
        owner = msg.sender;
    }

    function deposit() external payable {}

    // [VULN] tx.origin == owner e verdadeiro mesmo quando o chamador imediato
    // e um contrato intermedio (phishing). msg.sender seria diferente do owner.
    function withdraw(address payable to, uint256 amount) external {
        require(tx.origin == owner, "not owner");
        require(amount <= address(this).balance, "insufficient balance");

        (bool ok, ) = to.call{value: amount}("");
        require(ok, "transfer failed");
    }
}
