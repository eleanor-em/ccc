; ModuleID = 'primary'
source_filename = "primary"

@.format = private unnamed_addr constant [13 x i8] c"%lli + %llii\00", align 1

declare i32 @printf(i8*, ...)

define void @.iprint(i64 %0, i64 %1) {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.format, i32 0, i32 0), i64 %0, i64 %1)
  ret void
}

define void @main() {
entry:
  call void @.iprint(i64 1, i64 2)
  ret void
}
