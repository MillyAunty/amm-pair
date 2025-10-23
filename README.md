AMM Pair
AMM Pair is the core liquidity pool contract of a decentralized exchange on Stacks.
It implements a constant product AMM model (x * y = k) to maintain fair and automatic token swaps between two fungible tokens.

Features
Liquidity pool for two fungible tokens (FTs)
Add or remove liquidity for LP tokens
Swap tokens using constant product formula
Automatic fee deduction and price adjustment
Transparent event logging

Technical Overview
Language: Clarity
Model: Constant product (Uniswap-style)
Core Functions:
add-liquidity → Deposit both tokens to mint LP shares
remove-liquidity → Withdraw tokens by burning LP shares
swap → Exchange token A for token B (or vice versa)
get-reserves → Returns token balances in pool
get-price → View swap rate between tokens
Data Stored:
reserves-a, reserves-b – liquidity reserves
lp-balances – liquidity provider shares
total-liquidity – total LP token supply

Installation & Usage
Clone repository:
git clone https://github.com/your-repo/amm-pair.git
cd amm-pair
Deploy using Clarinet:
clarinet contract deploy amm-pair
Run tests:
clarinet test
