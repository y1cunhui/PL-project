// SPDX-License-Identifier: UNLICENSED
pragma solidity >=0.8.0;

contract Reentrancy {
    mapping(address => uint256) balance;
    address owner;

    constructor() {
        owner = msg.sender;
    }

    function deposit(uint value) public payable {
        if (msg.value >= value)
            balance[msg.sender] += value;
    }

    function withdraw(uint num) public {
        require(balance[msg.sender]>num);
        balance[msg.sender] -= num;
            payable(msg.sender).transfer(num);
    }
}