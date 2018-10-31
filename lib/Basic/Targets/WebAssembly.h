//=== WebAssembly.h - Declare WebAssembly target feature support *- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares WebAssembly TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_WEBASSEMBLY_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_WEBASSEMBLY_H

#include "Targets.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/Compiler.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY WebAssemblyTargetInfo : public TargetInfo {
  static const Builtin::Info BuiltinInfo[];

  enum SIMDEnum {
    NoSIMD,
    SIMD128,
  } SIMDLevel;

  bool HasNontrappingFPToInt;
  bool HasSignExt;
  bool HasExceptionHandling;

public:
  explicit WebAssemblyTargetInfo(const llvm::Triple &T, const TargetOptions &)
      : TargetInfo(T), SIMDLevel(NoSIMD), HasNontrappingFPToInt(false),
        HasSignExt(false), HasExceptionHandling(false) {
    NoAsmVariants = true;
    SuitableAlign = 128;
    LargeArrayMinWidth = 128;
    LargeArrayAlign = 128;
    SimdDefaultAlign = 128;
    SigAtomicType = SignedLong;
    LongDoubleWidth = LongDoubleAlign = 128;
    LongDoubleFormat = &llvm::APFloat::IEEEquad();
    MaxAtomicPromoteWidth = MaxAtomicInlineWidth = 64;
    // size_t being unsigned long for both wasm32 and wasm64 makes mangled names
    // more consistent between the two.
    SizeType = UnsignedLong;
    PtrDiffType = SignedLong;
    IntPtrType = SignedLong;
  }

protected:
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

private:
  bool
  initFeatureMap(llvm::StringMap<bool> &Features, DiagnosticsEngine &Diags,
                 StringRef CPU,
                 const std::vector<std::string> &FeaturesVec) const override {
    if (CPU == "bleeding-edge") {
      Features["simd128"] = true;
      Features["nontrapping-fptoint"] = true;
      Features["sign-ext"] = true;
    }
    return TargetInfo::initFeatureMap(Features, Diags, CPU, FeaturesVec);
  }

  bool hasFeature(StringRef Feature) const final;

  bool handleTargetFeatures(std::vector<std::string> &Features,
                            DiagnosticsEngine &Diags) final;

  bool isValidCPUName(StringRef Name) const final;
  void fillValidCPUList(SmallVectorImpl<StringRef> &Values) const final;

  bool setCPU(const std::string &Name) final { return isValidCPUName(Name); }

  ArrayRef<Builtin::Info> getTargetBuiltins() const final;

  BuiltinVaListKind getBuiltinVaListKind() const final {
    return VoidPtrBuiltinVaList;
  }

  ArrayRef<const char *> getGCCRegNames() const final { return None; }

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const final {
    return None;
  }

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &Info) const final {
    return false;
  }

  const char *getClobbers() const final { return ""; }

  bool isCLZForZeroUndef() const final { return false; }

  bool hasInt128Type() const final { return true; }

  IntType getIntTypeByWidth(unsigned BitWidth, bool IsSigned) const final {
    // WebAssembly prefers long long for explicitly 64-bit integers.
    return BitWidth == 64 ? (IsSigned ? SignedLongLong : UnsignedLongLong)
                          : TargetInfo::getIntTypeByWidth(BitWidth, IsSigned);
  }

  IntType getLeastIntTypeByWidth(unsigned BitWidth, bool IsSigned) const final {
    // WebAssembly uses long long for int_least64_t and int_fast64_t.
    return BitWidth == 64
               ? (IsSigned ? SignedLongLong : UnsignedLongLong)
               : TargetInfo::getLeastIntTypeByWidth(BitWidth, IsSigned);
  }
};
class LLVM_LIBRARY_VISIBILITY WebAssembly32TargetInfo
    : public WebAssemblyTargetInfo {
  std::unique_ptr<TargetInfo> HostTarget;
public:
  explicit WebAssembly32TargetInfo(const llvm::Triple &T,
                                   const TargetOptions &Opts)
      : WebAssemblyTargetInfo(T, Opts) {
    if (Opts.HostTriple == "") {
      HostTarget = nullptr;
    } else {
      llvm::Triple HostTriple(Opts.HostTriple);
      HostTarget.reset(AllocateTarget(llvm::Triple(Opts.HostTriple), Opts));

      // We want C/C++ to follow a different machine model from the 
      // target(wasm32). This different machine model is used when C/C++ is
      // lowered to llvm. However, the produced llvm is still lowered to WASM32
      // with WASM32's machine model

      // Properties from host target that we can safely copy
      PointerAlign = HostTarget->getPointerAlign(/* AddrSpace = */ 0);
      BoolWidth = HostTarget->getBoolWidth();
      BoolAlign = HostTarget->getBoolAlign();
      IntWidth = HostTarget->getIntWidth();
      IntAlign = HostTarget->getIntAlign();
      HalfWidth = HostTarget->getHalfWidth();
      HalfAlign = HostTarget->getHalfAlign();
      FloatWidth = HostTarget->getFloatWidth();
      FloatAlign = HostTarget->getFloatAlign();
      DoubleWidth = HostTarget->getDoubleWidth();
      DoubleAlign = HostTarget->getDoubleAlign();
      LongWidth = HostTarget->getLongWidth();
      LongAlign = HostTarget->getLongAlign();
      LongLongWidth = HostTarget->getLongLongWidth();
      LongLongAlign = HostTarget->getLongLongAlign();
      MinGlobalAlign = HostTarget->getMinGlobalAlign();
      DefaultAlignForAttributeAligned = 
        HostTarget->getDefaultAlignForAttributeAligned();
      IntMaxType = HostTarget->getIntMaxType();
      WCharType = HostTarget->getWCharType();
      WIntType = HostTarget->getWIntType();
      Char16Type = HostTarget->getChar16Type();
      Char32Type = HostTarget->getChar32Type();
      Int64Type = HostTarget->getInt64Type();
      ProcessIDType = HostTarget->getProcessIDType();
      LongDoubleWidth = HostTarget->getLongDoubleWidth();
      LongDoubleAlign = HostTarget->getLongDoubleAlign();
      UseBitFieldTypeAlignment = HostTarget->useBitFieldTypeAlignment();
      UseZeroLengthBitfieldAlignment = 
        HostTarget->useZeroLengthBitfieldAlignment();
      UseExplicitBitFieldAlignment = 
        HostTarget->useExplicitBitFieldAlignment();
      ZeroLengthBitfieldBoundary = 
        HostTarget->getZeroLengthBitfieldBoundary();
      LargeArrayMinWidth = HostTarget->getLargeArrayMinWidth();
      LargeArrayAlign = HostTarget->getLargeArrayAlign();

      if (LongWidth == 32) {
        SizeType = UnsignedLong;
        PtrDiffType = SignedLong;
        IntPtrType = SignedLong;
      } else if (IntWidth == 32) {
        SizeType = UnsignedInt;
        PtrDiffType = SignedInt;
        IntPtrType = SignedInt;
      }

      if (PointerWidth <= HostTarget->getPointerWidth(/* AddrSpace = */ 0)) {
        TrailingPointerPadding = HostTarget->getPointerWidth(/* AddrSpace = */ 0) - PointerWidth;
      }

      // We need to make sure wasm32 supports these properties before copying
      // from the host
      if (HostTarget->getMaxAtomicPromoteWidth() < MaxAtomicPromoteWidth) {
        MaxAtomicPromoteWidth = HostTarget->getMaxAtomicPromoteWidth();
      }

      if (HostTarget->getMaxAtomicInlineWidth() <= MaxAtomicInlineWidth) {
        MaxAtomicInlineWidth = HostTarget->getMaxAtomicInlineWidth();
      }

      if (HostTarget->getTypeWidth(HostTarget->getSigAtomicType()) <= 
        getTypeWidth(SigAtomicType)) {
        SigAtomicType = HostTarget->getSigAtomicType();
      }

      if (HostTarget->getNewAlign() > NewAlign) {
        NewAlign = HostTarget->getNewAlign();
      }

      if (HostTarget->getSuitableAlign() > SuitableAlign) {
        SuitableAlign = HostTarget->getSuitableAlign();
      }
      // Note we do not have to set the data layout differently, as llvm's
      // target layout is still the same
    }
    resetDataLayout("e-m:e-p:32:32-i64:64-n32:64-S128");
  }

  bool validateTarget(DiagnosticsEngine &Diags) const override {
    if (HostTarget != nullptr && HostTarget->isBigEndian() != isBigEndian()) {
      Diags.Report(diag::err_wasm_host_triple_unsupported_value) 
        << HostTarget->getTriple().str() << "Endian";
      return false;
    }
    if (LongWidth != 32 && IntWidth != 32) {
      Diags.Report(diag::err_wasm_host_triple_missing_integer_width) 
        << HostTarget->getTriple().str() << "32";
      return false;
    }
    if (PointerWidth > HostTarget->getPointerWidth(/* AddrSpace = */ 0)) {
      Diags.Report(diag::err_wasm_host_triple_has_small_pointer) 
        << HostTarget->getTriple().str()
        << std::to_string(HostTarget->getPointerWidth(/* AddrSpace = */ 0)) 
        << std::to_string(PointerWidth);
      return false;
    }

    return true;
  }

protected:
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;
};

class LLVM_LIBRARY_VISIBILITY WebAssembly64TargetInfo
    : public WebAssemblyTargetInfo {
public:
  explicit WebAssembly64TargetInfo(const llvm::Triple &T,
                                   const TargetOptions &Opts)
      : WebAssemblyTargetInfo(T, Opts) {
    LongAlign = LongWidth = 64;
    PointerAlign = PointerWidth = 64;
    SizeType = UnsignedLong;
    PtrDiffType = SignedLong;
    IntPtrType = SignedLong;
    resetDataLayout("e-m:e-p:64:64-i64:64-n32:64-S128");
  }

protected:
  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;
};
} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_WEBASSEMBLY_H
