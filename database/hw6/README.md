# Составьте запросы в терминах языков Datalog и SQL для базы данных «Университет», позволяющие получать

## Информацию о студентах

1. С заданным ФИО (*StudentId*, *StudentName*, *GroupId* по *:StudentName*).  
2. Учащихся в заданной группе (*StudentId*, *StudentName*, *GroupId* по *:GroupName*).  
3. Учащихся на заданном факультете (*StudentId*, *StudentName*, *GroupId* по *:FacultyName*).  
4. C заданной оценкой по дисциплине, заданной идентификатором (*StudentId*, *StudentName*, *GroupId* по *:Mark, :CourseId*).  
5. C заданной оценкой по дисциплине, заданной названием (*StudentId*, *StudentName*, *GroupId* по *:Mark, :CourseName*).

## Полную информацию о студентах

1. Для всех студентов (*StudentId*, *StudentName*, *GroupName*).  
2. Учащихся на заданном факультете (*StudentId*, *StudentName*, *GroupName* по *:FacultyName*).  
3. C факультета, заданного деканом (*StudentId*, *StudentName*, *GroupName* по *:LecturerName*).  
4. Студентов, не имеющих оценки по дисциплине, заданной идентификатором (*StudentId*, *StudentName*, *GroupName* по *:CourseId*).  
5. Студентов, не имеющих оценки по дисциплине, заданной названием (*StudentId*, *StudentName*, *GroupName* по *:CourseName*).  
6. Студентов, не имеющих оценки по дисциплине, у которых есть эта дисциплина (*StudentId*, *StudentName*, *GroupName* по *:CourseId*).  
7. Студентов, не имеющих оценки по дисциплине, у которых есть эта дисциплина (*StudentId*, *StudentName*, *GroupName* по *:CourseName*).

## Студенты и дисциплины, такие что у студента была дисциплина (по плану или есть оценка)

1. Идентификаторы (*StudentId*, *CourseId*).  
2. Имя и название (*StudentName*, *CourseName*).  
3. Имя и название, преподаватель того же факультета (*StudentName*, *CourseName*).  
4. Имя и название, преподаватель другого факультета (*StudentName*, *CourseName*).

## Студенты и дисциплины, такие что дисциплина есть в его плане, и у студента долг по этой дисциплине

1. Долгом считается отсутствие оценки (*StudentName*, *CourseName*).  
2. Долгом считается оценка не выше 2 (*StudentName*, *CourseName*).  
3. Долгом считается отсутствие оценки или оценка не выше 2 (*StudentName*, *CourseName*).

## Идентификаторы студентов по преподавателю

1. Имеющих хотя бы одну оценку у преподавателя (*StudentId* по *:LecturerName*).  
2. Не имеющих ни одной оценки у преподавателя (*StudentId* по *:LecturerName*).  
3. Имеющих оценки по всем дисциплинам преподавателя (*StudentId* по *:LecturerName*).  
4. Имеющих оценки по всем дисциплинам преподавателя, которые он вёл у этого студента (*StudentId* по *:LecturerName*).

## Группы и дисциплины, такие что все студенты группы имеют оценку по предмету

1. Идентификаторы (*GroupId*, *CourseId*).  
2. Названия (*GroupName*, *CourseName*).
