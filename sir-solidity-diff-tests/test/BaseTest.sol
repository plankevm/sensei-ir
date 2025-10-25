// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";

/// @author philogy <https://github.com/philogy>
abstract contract BaseTest is Test {
    function ffi(bytes memory encodedArgs) internal returns (bytes memory) {
        uint256 totalArgs;
        assembly ("memory-safe") {
            let firstOffset := mload(add(encodedArgs, 0x20))
            totalArgs := div(firstOffset, 0x20)
        }
        string[] memory args =
            abi.decode(bytes.concat(bytes32(uint256(0x20)), bytes32(totalArgs), encodedArgs), (string[]));
        return vm.ffi(args);
    }

    function sir(bytes memory encodedSirArgs) internal returns (bytes memory) {
        uint256 totalArgs;
        assembly ("memory-safe") {
            let firstOffset := mload(add(encodedSirArgs, 0x20))
            totalArgs := div(firstOffset, 0x20)
        }
        string[] memory sirArgs =
            abi.decode(bytes.concat(bytes32(uint256(0x20)), bytes32(totalArgs), encodedSirArgs), (string[]));
        string[] memory args = new string[](5 + sirArgs.length);
        string[5] memory runSir = ["cargo", "run", "-p", "sir-cli", "--"];
        for (uint256 i = 0; i < runSir.length; i++) {
            args[i] = runSir[i];
        }
        for (uint256 i = 0; i < sirArgs.length; i++) {
            args[i + runSir.length] = sirArgs[i];
        }
        return vm.ffi(args);
    }
}
