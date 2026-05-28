// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Tokens ERC20 nao-conformes (ex: USDT) retornam false em vez de reverter.
interface IERC20Minimal {
    function transfer(address to, uint256 amount) external returns (bool);
}

// [VULN] OWASP-06 variante 2: Unchecked ERC20 transfer()
// Ao contrario de ETH, tokens ERC20 podem falhar silenciosamente (retornam false).
// Ignorar o valor de retorno de transfer() permite que o contrato debite estado
// interno mesmo quando os tokens nunca chegam ao destinatario.
contract MVE_O6_Vuln2 {
    IERC20Minimal public immutable token;
    mapping(address => uint256) public allocated;
    address public owner;

    error NotOwner();

    constructor(address _token) {
        token = IERC20Minimal(_token);
        owner = msg.sender;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner();
        _;
    }

    function allocate(address to, uint256 amount) external onlyOwner {
        allocated[to] += amount;
    }

    function claim() external {
        uint256 amount = allocated[msg.sender];
        require(amount > 0, "nothing to claim");
        allocated[msg.sender] = 0;

        // [VULN] Ignora o valor de retorno de transfer().
        // Tokens USDT-style retornam false sem reverter.
        // O estado ja foi zerado mas os tokens podem nunca ter sido enviados.
        token.transfer(msg.sender, amount);
    }
}
