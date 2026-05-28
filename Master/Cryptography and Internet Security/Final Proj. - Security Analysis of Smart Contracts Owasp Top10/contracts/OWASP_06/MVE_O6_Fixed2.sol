// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

interface IERC20Minimal {
    function transfer(address to, uint256 amount) external returns (bool);
}

// [FIX] OWASP-06 variante 2: Verificar sempre o retorno de ERC20 transfer()
// require() garante que a transacao reverte se o token retornar false,
// mantendo consistencia entre o estado interno e os saldos reais do token.
contract MVE_O6_Fixed2 {
    IERC20Minimal public immutable token;
    mapping(address => uint256) public allocated;
    address public owner;

    error NotOwner();
    error TransferFailed();

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

        // [FIX] Verifica o retorno de transfer() — reverte se o token retornar false.
        bool ok = token.transfer(msg.sender, amount);
        if (!ok) revert TransferFailed();
    }
}
