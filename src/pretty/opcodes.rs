use std::format;

pub fn to_str(opcode: u8) -> String {
    match opcode {
        0x0 => "STOP".to_string(),
        0x1 => "ADD".to_string(),
        0x2 => "MUL".to_string(),
        0x3 => "SUB".to_string(),
        0x4 => "DIV".to_string(),
        0x5 => "SDIV".to_string(),
        0x6 => "MOD".to_string(),
        0x7 => "SMOD".to_string(),
        0x8 => "ADDMOD".to_string(),
        0x9 => "MULMOD".to_string(),
        0xa => "EXP".to_string(),
        0xb => "SIGNEXTEND".to_string(),
        0x10 => "LT".to_string(),
        0x11 => "GT".to_string(),
        0x12 => "SLT".to_string(),
        0x13 => "SGT".to_string(),
        0x14 => "EQ".to_string(),
        0x15 => "ISZERO".to_string(),
        0x16 => "AND".to_string(),
        0x17 => "OR".to_string(),
        0x18 => "XOR".to_string(),
        0x19 => "NOT".to_string(),
        0x1a => "BYTE".to_string(),
        0x1b => "SHL".to_string(),
        0x1c => "SHR".to_string(),
        0x1d => "SAR".to_string(),
        0x20 => "SHA3".to_string(),
        0x30 => "ADDRESS".to_string(),
        0x31 => "BALANCE".to_string(),
        0x32 => "ORIGIN".to_string(),
        0x33 => "CALLER".to_string(),
        0x34 => "CALLVALUE".to_string(),
        0x35 => "CALLDATALOAD".to_string(),
        0x36 => "CALLDATASIZE".to_string(),
        0x37 => "CALLDATACOPY".to_string(),
        0x38 => "CODESIZE".to_string(),
        0x39 => "CODECOPY".to_string(),
        0x3a => "GASPRICE".to_string(),
        0x3b => "EXTCODESIZE".to_string(),
        0x3c => "EXTCODECOPY".to_string(),
        0x3d => "RETURNDATASIZE".to_string(),
        0x3e => "RETURNDATACOPY".to_string(),
        0x3f => "EXTCODEHASH".to_string(),
        0x40 => "BLOCKHASH".to_string(),
        0x41 => "COINBASE".to_string(),
        0x42 => "TIMESTAMP".to_string(),
        0x43 => "NUMBER".to_string(),
        0x44 => "DIFFICULTY".to_string(),
        0x45 => "GASLIMIT".to_string(),
        0x46 => "CHAINID".to_string(),
        0x47 => "SELFBALANCE".to_string(),
        0x48 => "BASEFEE".to_string(),
        0x50 => "POP".to_string(),
        0x51 => "MLOAD".to_string(),
        0x52 => "MSTORE".to_string(),
        0x53 => "MSTORE8".to_string(),
        0x54 => "SLOAD".to_string(),
        0x55 => "SSTORE".to_string(),
        0x56 => "JUMP".to_string(),
        0x57 => "JUMPI".to_string(),
        0x58 => "PC".to_string(),
        0x59 => "MSIZE".to_string(),
        0x5a => "GAS".to_string(),
        0x5b => "JUMPDEST".to_string(),
        0x5f..=0x7f => format!("PUSH{}", opcode - 0x5f),
        0x80..=0x8f => format!("DUP{}", opcode - 0x80 + 1),
        0x90..=0x9f => format!("SWAP{}", opcode - 0x90 + 1),
        0xa0..=0xa4 => format!("LOG{}", opcode - 0xa0),
        0xf0 => "CREATE".to_string(),
        0xf1 => "CALL".to_string(),
        0xf2 => "CALLCODE".to_string(),
        0xf3 => "RETURN".to_string(),
        0xf4 => "DELEGATECALL".to_string(),
        0xf5 => "CREATE2".to_string(),
        0xfa => "STATICCALL".to_string(),
        0xfd => "REVERT".to_string(),
        0xfe => "INVALID".to_string(),
        0xff => "SELFDESTRUCT".to_string(),
        _ => format!("{:x}", opcode),
    }
}

pub fn opcode_to_int(opcode: &str) -> Result<u8, ()> {
    match opcode.to_uppercase().as_ref() {
        "STOP" => Ok(0x0),
        "ADD" => Ok(0x1),
        "MUL" => Ok(0x2),
        "SUB" => Ok(0x3),
        "DIV" => Ok(0x4),
        "SDIV" => Ok(0x5),
        "MOD" => Ok(0x6),
        "SMOD" => Ok(0x7),
        "ADDMOD" => Ok(0x8),
        "MULMOD" => Ok(0x9),
        "EXP" => Ok(0xa),
        "SIGNEXTEND" => Ok(0xb),
        "LT" => Ok(0x10),
        "GT" => Ok(0x11),
        "SLT" => Ok(0x12),
        "SGT" => Ok(0x13),
        "EQ" => Ok(0x14),
        "ISZERO" => Ok(0x15),
        "AND" => Ok(0x16),
        "OR" => Ok(0x17),
        "XOR" => Ok(0x18),
        "NOT" => Ok(0x19),
        "BYTE" => Ok(0x1a),
        "SHL" => Ok(0x1b),
        "SHR" => Ok(0x1c),
        "SAR" => Ok(0x1d),
        "SHA3" => Ok(0x20),
        "ADDRESS" => Ok(0x30),
        "BALANCE" => Ok(0x31),
        "ORIGIN" => Ok(0x32),
        "CALLER" => Ok(0x33),
        "CALLVALUE" => Ok(0x34),
        "CALLDATALOAD" => Ok(0x35),
        "CALLDATASIZE" => Ok(0x36),
        "CALLDATACOPY" => Ok(0x37),
        "CODESIZE" => Ok(0x38),
        "CODECOPY" => Ok(0x39),
        "GASPRICE" => Ok(0x3a),
        "EXTCODESIZE" => Ok(0x3b),
        "EXTCODECOPY" => Ok(0x3c),
        "RETURNDATASIZE" => Ok(0x3d),
        "RETURNDATACOPY" => Ok(0x3e),
        "EXTCODEHASH" => Ok(0x3f),
        "BLOCKHASH" => Ok(0x40),
        "COINBASE" => Ok(0x41),
        "TIMESTAMP" => Ok(0x42),
        "NUMBER" => Ok(0x43),
        "PREVRANDAO" => Ok(0x44),
        "GASLIMIT" => Ok(0x45),
        "CHAINID" => Ok(0x46),
        "SELFBALANCE" => Ok(0x47),
        "BASEFEE" => Ok(0x48),
        "POP" => Ok(0x50),
        "MLOAD" => Ok(0x51),
        "MSTORE" => Ok(0x52),
        "MSTORE8" => Ok(0x53),
        "SLOAD" => Ok(0x54),
        "SSTORE" => Ok(0x55),
        "JUMP" => Ok(0x56),
        "JUMPI" => Ok(0x57),
        "PC" => Ok(0x58),
        "MSIZE" => Ok(0x59),
        "GAS" => Ok(0x5a),
        "JUMPDEST" => Ok(0x5b),
        push if push.starts_with("PUSH") => {
            if let Some(offset) = push.strip_prefix("PUSH").and_then(|x| x.parse::<u8>().ok()) {
                if offset <= 32 {
                    Ok(0x5f + offset)
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        }
        dup if dup.starts_with("DUP") => {
            if let Some(offset) = dup.strip_prefix("DUP").and_then(|x| x.parse::<u8>().ok()) {
                if (1..=16).contains(&offset) {
                    Ok(0x7f + offset)
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        }
        swap if swap.starts_with("SWAP") => {
            if let Some(offset) = swap.strip_prefix("SWAP").and_then(|x| x.parse::<u8>().ok()) {
                if (1..=16).contains(&offset) {
                    Ok(0x8f + offset)
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        }
        log if log.starts_with("LOG") => {
            if let Some(offset) = log.strip_prefix("LOG").and_then(|x| x.parse::<u8>().ok()) {
                if offset <= 4 {
                    Ok(0xa0 + offset)
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        }
        "CREATE" => Ok(0xf0),
        "CALL" => Ok(0xf1),
        "CALLCODE" => Ok(0xf2),
        "RETURN" => Ok(0xf3),
        "DELEGATECALL" => Ok(0xf4),
        "CREATE2" => Ok(0xf5),
        "STATICCALL" => Ok(0xfa),
        "REVERT" => Ok(0xfd),
        "INVALID" => Ok(0xfe),
        "SELFDESTRUCT" => Ok(0xff),
        _ => Err(()),
    }
}
