#!/usr/bin/env bash
#
# Canonical build entry for the Nemerle net10 retarget branch (Phase 5.2) --
# bash counterpart of build.ps1; same contract, same defaults.
#
# Drives the classic-MSBuild orchestrator NemerleAll.nproj. No VsDevCmd, no
# Visual Studio registry probing: the build host is located via --msbuild,
# the NEMERLE_MSBUILD environment variable, or vswhere.
#
# HOST SITUATION (probed 2026-10-31, see doc/NET10-BUILD-BASELINE.md):
#   `dotnet msbuild` (SDK 10) evaluates NemerleAll.nproj fine but fails to run
#   any stage: NemerleAll.nproj(157,5) error MSB4803 -- the task
#   "GetFrameworkSdkPath" (InitTools, a dependency of every stage) is not
#   supported on .NET Core MSBuild. Default host is therefore the
#   vswhere-discovered .NET Framework MSBuild.exe (>= 18, per global.json).
#   --use-dotnet-msbuild switches to `dotnet msbuild` for the day that
#   blocker is retired (Phase 6.3).
#
# WHAT DOES *NOT* WORK CROSS-PLATFORM TODAY (be honest -- this script is
# syntactically valid everywhere, but a full build is Windows-only until the
# phases listed for each item):
#   * Validate: uses Windows ildasm.exe and `fc` (the Unix branches inside
#     NemerleAll.nproj expect mono `monodis`/XBuild-era tooling; replaced by
#     net10 tooling in Phase 6.3).
#   * CompilerTests: runs the .NET Framework Nemerle.Compiler.Test.exe; the
#     mono ExeRunner fallback is legacy (XBuild era) and untested.
#   * Stage1: NTasks/Install machinery uses Windows-only helpers
#     (junction.exe) and net-4.0 sub-projects.
#   * The MSBuild host itself: vswhere/MSBuild.exe ship with Visual Studio
#     for Windows only.
#   On a non-Windows kernel this script therefore REFUSES to start a real
#   build (exit 2) instead of half-running it. Set
#   NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1 to try anyway at your own risk.
#   `--dry-run` always works and just prints the resolved command.
#
# Usage examples:
#   ./build.sh --targets Stage3,Validate --rebuild    # standard green gate
#   ./build.sh --targets Stage1,CompilerTests
#   ./build.sh --dry-run
set -u

usage() {
    cat <<'EOF'
Usage: build.sh [options]
  -c, --configuration CFG   Debug or Release (default: Debug)
  -t, --targets LIST        MSBuild targets, ',' or ';' separated (default:
                            Stage1;_PegAndCSharp;_ComputationExpressions;_Async;Install)
      --target-name NAME    Target for sub-projects: Build or Rebuild (default: Build)
      --rebuild             Shortcut for --target-name Rebuild
      --ninstall DIR        Install dir (default: <repo>/bin/<Configuration>/net-4.0)
      --verbosity V         MSBuild verbosity (default: m)
      --msbuild EXE         Explicit MSBuild.exe (overrides NEMERLE_MSBUILD, vswhere)
      --use-dotnet-msbuild  Use `dotnet msbuild` (currently blocked by MSB4803; fwd-compat)
      --dry-run             Print the resolved command and exit 0
  -h, --help                Show this help
EOF
}

configuration="Debug"
targets="Stage1,_PegAndCSharp,_ComputationExpressions,_Async,Install"
target_name="Build"
ninstall=""
verbosity="m"
msbuild_explicit=""
use_dotnet_msbuild=0
dry_run=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        -c|--configuration)   configuration="${2:?missing value for $1}"; shift 2 ;;
        -t|--targets)         targets="${2:?missing value for $1}"; shift 2 ;;
        --target-name)        target_name="${2:?missing value for $1}"; shift 2 ;;
        --rebuild)            target_name="Rebuild"; shift ;;
        --ninstall)           ninstall="${2:?missing value for $1}"; shift 2 ;;
        --verbosity)          verbosity="${2:?missing value for $1}"; shift 2 ;;
        --msbuild)            msbuild_explicit="${2:?missing value for $1}"; shift 2 ;;
        --use-dotnet-msbuild) use_dotnet_msbuild=1; shift ;;
        --dry-run)            dry_run=1; shift ;;
        -h|--help)            usage; exit 0 ;;
        *) echo "build.sh: unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
done

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project="$repo_root/NemerleAll.nproj"
[[ -f "$project" ]] || { echo "build.sh: NemerleAll.nproj not found next to build.sh ('$project')." >&2; exit 2; }

# Normalize targets: split on ',' or ';', rejoin with ',' (MSBuild syntax).
targets_joined="$(printf '%s' "$targets" | tr ',;' '\n\n' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//' | grep -v '^$' | paste -sd, -)"
[[ -n "$targets_joined" ]] || { echo "build.sh: target list is empty." >&2; exit 2; }

[[ -n "$ninstall" ]] || ninstall="$repo_root/bin/$configuration/net-4.0"
[[ -n "$target_name" ]] || target_name="Build"

uname_s="$(uname -s 2>/dev/null || echo unknown)"
case "$uname_s" in
    Windows_NT|MINGW*|MSYS*|CYGWIN*) is_windows=1 ;;
    *) is_windows=0 ;;
esac

cygpath_opt() { # $1: windows path (or plain path) -> unix path when possible
    if command -v cygpath >/dev/null 2>&1; then cygpath -u "$1" 2>/dev/null || printf '%s' "$1"; else printf '%s' "$1"; fi
}

resolve_msbuild() {
    local c vs_path p
    for c in "$msbuild_explicit" "${NEMERLE_MSBUILD:-}"; do
        [[ -z "$c" ]] && continue
        p="$(cygpath_opt "$c")"
        [[ -f "$p" ]] && { printf '%s' "$p"; return 0; }
        echo "build.sh: warning: MSBuild '$c' not found; continuing discovery." >&2
    done
    if [[ $is_windows -eq 1 ]]; then
        local pf86 vswhere
        pf86="$(printenv 'ProgramFiles(x86)' 2>/dev/null || true)"
        [[ -n "$pf86" ]] || pf86='C:\Program Files (x86)'
        vswhere="$(cygpath_opt "$pf86")/Microsoft Visual Studio/Installer/vswhere.exe"
        if [[ -x "$vswhere" ]]; then
            vs_path="$("$vswhere" -latest -products '*' -requires Microsoft.Component.MSBuild -property installationPath | head -n1 | tr -d '\r')"
            if [[ -n "$vs_path" ]]; then
                p="$(cygpath_opt "$vs_path")/MSBuild/Current/Bin/MSBuild.exe"
                [[ -f "$p" ]] && { printf '%s' "$p"; return 0; }
            fi
        fi
    fi
    return 1
}

# Required by NemerleAll.nproj property functions (same as the old .cmd entry).
export MSBUILDENABLEALLPROPERTYFUNCTIONS=1

msbuild_args=(
    "$project"
    "-t:$targets_joined"
    "-p:NTargetName=$target_name"
    "-p:Configuration=$configuration"
    "-p:NInstall=$ninstall"
    "-v:$verbosity"
    "-nodereuse:false"   # do not leave MSBuild nodes holding file locks between builds
)

if [[ $use_dotnet_msbuild -eq 1 ]]; then
    exe="dotnet"; exe_args=("msbuild" "${msbuild_args[@]}")
else
    if exe="$(resolve_msbuild)"; then
        exe_args=("${msbuild_args[@]}")
    else
        exe="<MSBuild.exe unresolved>"
        exe_args=("${msbuild_args[@]}")
        if [[ $dry_run -eq 0 ]]; then
            echo "build.sh: no MSBuild host found. On Windows install Visual Studio with the MSBuild component (vswhere discovery) or pass --msbuild; on Linux/macOS the build is not supported yet (see header)." >&2
            exit 2
        fi
    fi
fi

echo "Nemerle build: Configuration=$configuration NTargetName=$target_name"
echo "  targets:  $targets_joined"
echo "  install:  $ninstall"
printf '  command:  %q' "$exe"; printf ' %q' "${exe_args[@]}"; printf '\n'

if [[ $dry_run -eq 1 ]]; then
    echo "DryRun: not executing."
    exit 0
fi

if [[ $is_windows -eq 0 && "${NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS:-0}" != "1" ]]; then
    cat >&2 <<'EOF'
build.sh: a real build requires Windows today (MSBuild.exe host, ildasm/fc in
Validate, .NET Framework CompilerTests harness -- see the header comment of
this file). Nothing was executed. Re-run with --dry-run to inspect the
command, or set NEMERLE_BUILD_ALLOW_UNSUPPORTED_OS=1 to attempt anyway.
EOF
    exit 2
fi

if [[ $use_dotnet_msbuild -eq 0 && ! -x "$exe" ]]; then
    echo "build.sh: resolved host '$exe' is not executable." >&2
    exit 2
fi

"$exe" "${exe_args[@]}"
code=$?
if [[ $code -eq 0 ]]; then
    echo "Build succeeded. Nemerle installed to '$ninstall' (when the Install target ran)."
else
    echo "Build FAILED with exit code $code."
fi
exit $code
