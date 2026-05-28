; ModuleID = 'get_sign.bc'
source_filename = "get_sign.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [2 x i8] c"a\00", align 1, !dbg !0

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @get_sign(i32 noundef %x) #0 !dbg !17 {
entry:
  %retval = alloca i32, align 4
  %x.addr = alloca i32, align 4
  store i32 %x, ptr %x.addr, align 4
  call void @llvm.dbg.declare(metadata ptr %x.addr, metadata !22, metadata !DIExpression()), !dbg !23
  %0 = load i32, ptr %x.addr, align 4, !dbg !24
  %cmp = icmp eq i32 %0, 0, !dbg !26
  br i1 %cmp, label %if.then, label %if.end, !dbg !27

if.then:                                          ; preds = %entry
  store i32 0, ptr %retval, align 4, !dbg !28
  br label %return, !dbg !28

if.end:                                           ; preds = %entry
  %1 = load i32, ptr %x.addr, align 4, !dbg !29
  %cmp1 = icmp slt i32 %1, 0, !dbg !31
  br i1 %cmp1, label %if.then2, label %if.else, !dbg !32

if.then2:                                         ; preds = %if.end
  store i32 -1, ptr %retval, align 4, !dbg !33
  br label %return, !dbg !33

if.else:                                          ; preds = %if.end
  store i32 1, ptr %retval, align 4, !dbg !34
  br label %return, !dbg !34

return:                                           ; preds = %if.else, %if.then2, %if.then
  %2 = load i32, ptr %retval, align 4, !dbg !35
  ret i32 %2, !dbg !35
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main() #0 !dbg !36 {
entry:
  %retval = alloca i32, align 4
  %a = alloca i32, align 4
  store i32 0, ptr %retval, align 4
  call void @llvm.dbg.declare(metadata ptr %a, metadata !39, metadata !DIExpression()), !dbg !40
  call void @klee_make_symbolic(ptr noundef %a, i64 noundef 4, ptr noundef @.str), !dbg !41
  %0 = load i32, ptr %a, align 4, !dbg !42
  %call = call i32 @get_sign(i32 noundef %0), !dbg !43
  ret i32 %call, !dbg !44
}

declare void @klee_make_symbolic(ptr noundef, i64 noundef, ptr noundef) #2

attributes #0 = { noinline nounwind uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!7}
!llvm.module.flags = !{!9, !10, !11, !12, !13, !14, !15}
!llvm.ident = !{!16}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(scope: null, file: !2, line: 17, type: !3, isLocal: true, isDefinition: true)
!2 = !DIFile(filename: "get_sign.c", directory: "/home/klee/week11/week11", checksumkind: CSK_MD5, checksum: "bb8df99687a4a359e9274fa34e0963ba")
!3 = !DICompositeType(tag: DW_TAG_array_type, baseType: !4, size: 16, elements: !5)
!4 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!5 = !{!6}
!6 = !DISubrange(count: 2)
!7 = distinct !DICompileUnit(language: DW_LANG_C11, file: !2, producer: "clang version 16.0.6 (https://github.com/llvm/llvm-project.git 7cbf1a2591520c2491aa35339f227775f4d3adf6)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, globals: !8, splitDebugInlining: false, nameTableKind: None)
!8 = !{!0}
!9 = !{i32 7, !"Dwarf Version", i32 5}
!10 = !{i32 2, !"Debug Info Version", i32 3}
!11 = !{i32 1, !"wchar_size", i32 4}
!12 = !{i32 8, !"PIC Level", i32 2}
!13 = !{i32 7, !"PIE Level", i32 2}
!14 = !{i32 7, !"uwtable", i32 2}
!15 = !{i32 7, !"frame-pointer", i32 2}
!16 = !{!"clang version 16.0.6 (https://github.com/llvm/llvm-project.git 7cbf1a2591520c2491aa35339f227775f4d3adf6)"}
!17 = distinct !DISubprogram(name: "get_sign", scope: !2, file: !2, line: 3, type: !18, scopeLine: 3, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !21)
!18 = !DISubroutineType(types: !19)
!19 = !{!20, !20}
!20 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!21 = !{}
!22 = !DILocalVariable(name: "x", arg: 1, scope: !17, file: !2, line: 3, type: !20)
!23 = !DILocation(line: 3, column: 18, scope: !17)
!24 = !DILocation(line: 4, column: 7, scope: !25)
!25 = distinct !DILexicalBlock(scope: !17, file: !2, line: 4, column: 7)
!26 = !DILocation(line: 4, column: 9, scope: !25)
!27 = !DILocation(line: 4, column: 7, scope: !17)
!28 = !DILocation(line: 5, column: 5, scope: !25)
!29 = !DILocation(line: 7, column: 7, scope: !30)
!30 = distinct !DILexicalBlock(scope: !17, file: !2, line: 7, column: 7)
!31 = !DILocation(line: 7, column: 9, scope: !30)
!32 = !DILocation(line: 7, column: 7, scope: !17)
!33 = !DILocation(line: 8, column: 5, scope: !30)
!34 = !DILocation(line: 10, column: 5, scope: !30)
!35 = !DILocation(line: 11, column: 1, scope: !17)
!36 = distinct !DISubprogram(name: "main", scope: !2, file: !2, line: 13, type: !37, scopeLine: 14, spFlags: DISPFlagDefinition, unit: !7, retainedNodes: !21)
!37 = !DISubroutineType(types: !38)
!38 = !{!20}
!39 = !DILocalVariable(name: "a", scope: !36, file: !2, line: 15, type: !20)
!40 = !DILocation(line: 15, column: 7, scope: !36)
!41 = !DILocation(line: 17, column: 3, scope: !36)
!42 = !DILocation(line: 19, column: 19, scope: !36)
!43 = !DILocation(line: 19, column: 10, scope: !36)
!44 = !DILocation(line: 19, column: 3, scope: !36)
