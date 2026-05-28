// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// [VULN] OWASP-08 variante 2: Cross-function reentrancy
// O estado (balanceOf) e partilhado entre withdraw() e transfer().
// withdraw() faz uma chamada externa ANTES de atualizar o saldo.
// Durante o callback, o atacante invoca transfer() que ainda ve o saldo antigo
// e permite uma transferencia que nao deveria ser possivel.
contract MVE_O8_Vuln2 {
    mapping(address => uint256) public balanceOf;

    function deposit() external payable {
        balanceOf[msg.sender] += msg.value;
    }

    function withdraw(uint256 amount) external {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");

        // [VULN] Chamada externa antes de atualizar estado.
        // Durante o callback, balanceOf[msg.sender] ainda tem o valor antigo.
        (bool ok, ) = msg.sender.call{value: amount}("");
        require(ok, "transfer failed");

        balanceOf[msg.sender] -= amount;
    }

    // [VULN] Le balanceOf sem qualquer guard — pode ser chamado durante o callback
    // de withdraw(), usando o saldo stale para transferir tokens extra.
    function transfer(address to, uint256 amount) external {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");
        balanceOf[msg.sender] -= amount;
        balanceOf[to] += amount;
    }
}
