// Safe shell command execution module
// Provides type-safe wrappers for common shell operations
// All arguments are properly sanitized before execution

import gleam/string
import gleam/list
import gleam/int
import gleam/option.{type Option, None, Some}
import vibee/mcp/validation

// ============================================================
// External Erlang FFI for shell execution
// ============================================================

@external(erlang, "vibee_shell_ffi", "safe_cmd")
fn safe_os_cmd(cmd: String) -> Result(String, String)

// ============================================================
// Core Execution Function
// ============================================================

/// Execute a shell command and return the output
pub fn exec(cmd: String) -> Result(String, String) {
  safe_os_cmd(cmd)
}

/// Execute command with timeout (in seconds)
pub fn exec_with_timeout(cmd: String, timeout_seconds: Int) -> Result(String, String) {
  let timeout_cmd = "timeout " <> int.to_string(timeout_seconds) <> " " <> cmd
  exec(timeout_cmd)
}

// ============================================================
// Argument Sanitization
// ============================================================

/// Sanitize a single argument for shell (wrap in single quotes)
pub fn sanitize_arg(arg: String) -> String {
  validation.sanitize_for_shell(arg)
}

/// Build command string with sanitized arguments
pub fn build_command(cmd: String, args: List(String)) -> String {
  let safe_args = list.map(args, sanitize_arg)
  cmd <> " " <> string.join(safe_args, " ")
}

// ============================================================
// Gleam Build Commands
// ============================================================

/// Execute gleam build safely
pub fn gleam_build(path: String, target: String) -> Result(String, String) {
  // Validate and sanitize path
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      // Whitelist target values
      let safe_target = validation.validate_build_target(Some(target))
      let quoted_path = sanitize_arg(safe_path)

      let cmd = "cd " <> quoted_path <> " && gleam build --target=" <> safe_target <> " 2>&1"
      exec(cmd)
    }
  }
}

/// Execute gleam test safely
pub fn gleam_test(path: String, filter: Option(String)) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let quoted_path = sanitize_arg(safe_path)

      let cmd = case filter {
        None -> "cd " <> quoted_path <> " && gleam test 2>&1"
        Some(f) -> {
          let safe_filter = validation.sanitize_string(f)
          "cd " <> quoted_path <> " && gleam test " <> sanitize_arg(safe_filter) <> " 2>&1"
        }
      }

      exec(cmd)
    }
  }
}

/// Execute gleam run safely
pub fn gleam_run(path: String, module: Option(String)) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let quoted_path = sanitize_arg(safe_path)

      let cmd = case module {
        None -> "cd " <> quoted_path <> " && gleam run 2>&1"
        Some(m) -> {
          let safe_module = validation.sanitize_string(m)
          "cd " <> quoted_path <> " && gleam run -m " <> sanitize_arg(safe_module) <> " 2>&1"
        }
      }

      exec(cmd)
    }
  }
}

// ============================================================
// Git Commands
// ============================================================

/// Execute git diff safely
pub fn git_diff(path: String, mode: Option(String), other_file: Option(String)) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let quoted_path = sanitize_arg(safe_path)

      let cmd = case mode, other_file {
        Some("staged"), _ -> "git diff --staged -- " <> quoted_path <> " 2>&1"
        Some("git_staged"), _ -> "git diff --staged -- " <> quoted_path <> " 2>&1"
        Some("git_head"), _ -> "git diff HEAD -- " <> quoted_path <> " 2>&1"
        Some("custom"), Some(other) -> {
          case validation.validate_path(other) {
            Ok(safe_other) -> "diff -u " <> quoted_path <> " " <> sanitize_arg(safe_other) <> " 2>&1"
            Error(_) -> "echo 'Invalid other file path'"
          }
        }
        Some("backup"), _ -> "diff -u " <> quoted_path <> " " <> quoted_path <> ".bak 2>&1"
        _, _ -> "git diff HEAD -- " <> quoted_path <> " 2>&1"
      }

      exec(cmd)
    }
  }
}

/// Execute git status
pub fn git_status(path: Option(String)) -> Result(String, String) {
  let cmd = case path {
    None -> "git status 2>&1"
    Some(p) -> {
      case validation.validate_path(p) {
        Ok(safe_path) -> "cd " <> sanitize_arg(safe_path) <> " && git status 2>&1"
        Error(_) -> "git status 2>&1"
      }
    }
  }

  exec(cmd)
}

// ============================================================
// Search Commands
// ============================================================

/// Execute grep safely
pub fn grep_files(pattern: String, path: String, file_type: Option(String)) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let safe_pattern = validation.sanitize_string(pattern)
      let quoted_pattern = sanitize_arg(safe_pattern)
      let quoted_path = sanitize_arg(safe_path)

      let include_opt = case file_type {
        None -> ""
        Some(ft) -> {
          let safe_type = validation.sanitize_string(ft)
          " --include=" <> sanitize_arg("*." <> safe_type)
        }
      }

      let cmd = "grep -r -l " <> quoted_pattern <> " " <> quoted_path <> include_opt <> " 2>/dev/null | head -20"
      exec(cmd)
    }
  }
}

/// Execute find command safely
pub fn find_files(path: String, pattern: Option(String)) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let quoted_path = sanitize_arg(safe_path)

      let cmd = case pattern {
        None -> "find " <> quoted_path <> " -type f 2>/dev/null | head -50"
        Some(p) -> {
          let safe_pattern = validation.sanitize_string(p)
          "find " <> quoted_path <> " -type f -name " <> sanitize_arg(safe_pattern) <> " 2>/dev/null | head -50"
        }
      }

      exec(cmd)
    }
  }
}

/// Count files matching pattern
pub fn count_files(path: String, pattern: String) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let safe_pattern = validation.sanitize_string(pattern)
      let cmd = "find " <> sanitize_arg(safe_path) <> " -name " <> sanitize_arg(safe_pattern) <> " 2>/dev/null | wc -l"
      exec(cmd)
    }
  }
}

// ============================================================
// File Commands
// ============================================================

/// Read file contents safely (use simplifile instead when possible)
pub fn cat_file(path: String) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let cmd = "cat " <> sanitize_arg(safe_path) <> " 2>&1"
      exec(cmd)
    }
  }
}

/// Get file info
pub fn file_stat(path: String) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let cmd = "stat " <> sanitize_arg(safe_path) <> " 2>&1"
      exec(cmd)
    }
  }
}

/// List directory contents
pub fn ls_directory(path: String) -> Result(String, String) {
  case validation.validate_path(path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let cmd = "ls -la " <> sanitize_arg(safe_path) <> " 2>&1"
      exec(cmd)
    }
  }
}

// ============================================================
// Voice/Media Commands
// ============================================================

/// Execute whisper transcription safely
pub fn whisper_transcribe(file_path: String, language: Option(String)) -> Result(String, String) {
  case validation.validate_path(file_path) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(safe_path) -> {
      let quoted_path = sanitize_arg(safe_path)

      let lang_opt = case language {
        None -> ""
        Some(l) -> {
          // Whitelist language codes
          let safe_lang = case l {
            "ru" | "en" | "de" | "fr" | "es" | "it" | "pt" | "zh" | "ja" | "ko" -> l
            _ -> "ru"  // Default to Russian
          }
          " --language " <> safe_lang
        }
      }

      let cmd = "whisper " <> quoted_path <> lang_opt <> " --model tiny 2>&1"
      exec(cmd)
    }
  }
}

// ============================================================
// System Commands (Restricted)
// ============================================================

/// Execute allowed system command
pub fn system_exec(command: String, args: Option(List(String)), timeout: Option(Int)) -> Result(String, String) {
  // Validate command against whitelist
  case validation.validate_command(command) {
    Error(e) -> Error(validation.error_to_string(e))
    Ok(cmd) -> {
      let safe_args = case args {
        None -> ""
        Some(arg_list) -> {
          let sanitized = list.map(arg_list, fn(a) {
            sanitize_arg(validation.sanitize_string(a))
          })
          " " <> string.join(sanitized, " ")
        }
      }

      let full_cmd = cmd <> safe_args <> " 2>&1"

      case timeout {
        None -> exec(full_cmd)
        Some(t) -> exec_with_timeout(full_cmd, t)
      }
    }
  }
}

// ============================================================
// Test Coverage Commands
// ============================================================

/// Count source files in directory
pub fn count_source_files(path: String) -> Result(String, String) {
  count_files(path <> "/src", "*.gleam")
}

/// Count test files in directory
pub fn count_test_files(path: String) -> Result(String, String) {
  count_files(path <> "/test", "*.gleam")
}

// ============================================================
// Utility Functions
// ============================================================

/// Check if path exists
pub fn path_exists(path: String) -> Bool {
  case validation.validate_path(path) {
    Error(_) -> False
    Ok(safe_path) -> {
      let cmd = "test -e " <> sanitize_arg(safe_path) <> " && echo 'exists' || echo 'not_found'"
      case exec(cmd) {
        Ok(output) -> string.contains(output, "exists")
        Error(_) -> False
      }
    }
  }
}

/// Get current timestamp
pub fn get_timestamp() -> String {
  case exec("date '+%Y-%m-%d %H:%M:%S'") {
    Ok(ts) -> string.trim(ts)
    Error(_) -> "unknown"
  }
}
