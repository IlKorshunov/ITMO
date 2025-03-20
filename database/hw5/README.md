## Домашнее задание 5. Реляционная алгебра

Структура базы данных «Университет»:

- *Faculties(FacultyId, FacultyName, DeanId)*
- *Groups(GroupId, GroupName, GroupFacultyId)*
- *Students(StudentId, StudentName, GroupId)*
- *Courses(CourseId, CourseName)*
- *Lecturers(LecturerId, LecturerName, LecturerFacultyId)*
- *Plan(GroupId, CourseId, LecturerId)*
- *Marks(StudentId, CourseId, Mark)*

Составьте выражения реляционной алгебры и соответствующие SQL-запросы, позволяющие получать

1. Информацию о студентах
    1. С заданным идентификатором (*StudentId*, *StudentName*, *GroupId* по *:StudentId*).
    2. С заданным ФИО (*StudentId*, *StudentName*, *GroupId* по *:StudentName*).
2. Полную информацию о студентах
    1. С заданным идентификатором (*StudentId*, *StudentName*, *GroupName* по *:StudentId*).  
    2. С заданным ФИО (*StudentId*, *StudentName*, *GroupName* по *:StudentName*).  
    3. Из заданной группы (*StudentId*, *StudentName*, *GroupName* по *:GroupName*).  
    4. C заданного факультета (*StudentId*, *StudentName*, *GroupName* по *:FacultyName*).  
    5. C факультета, заданного деканом (*StudentId*, *StudentName*, *GroupName* по *:LecturerName*).
3. Информацию о студентах с заданной оценкой по дисциплине
    1. С заданным идентификатором (*StudentId*, *StudentName*, *GroupId* по *:Mark, :CourseId*).  
    2. С заданным названием (*StudentId*, *StudentName*, *GroupId* по *:Mark, :CourseName*).  
    3. Которую у него вёл лектор заданный идентификатором (*StudentId*, *StudentName*, *GroupId* по *:Mark, :LecturerId*).  
    4. Которую у них вёл лектор, заданный ФИО (*StudentId*, *StudentName*, *GroupId* по *:Mark, :LecturerName*).  
    5. Которую вёл лектор, заданный идентификатором (*StudentId*, *StudentName*, *GroupId* по *:Mark, :LecturerId*).  
    6. Которую вёл лектор, заданный ФИО (*StudentId*, *StudentName*, *GroupId* по *:Mark, :LecturerName*).
4. Информацию о студентах не имеющих оценки по дисциплине
    1. Среди всех студентов (*StudentId*, *StudentName*, *GroupId* по *:CourseName*).  
    2. Тут был дубль задачи, пункт оставлен для сохранения нумерации.  
    3. Среди студентов факультета (*StudentId*, *StudentName*, *GroupId* по *:CourseName, :FacultyName*).  
    4. Среди студентов, у которых есть эта дисциплина (*StudentId*, *StudentName*, *GroupId* по *:CourseName*).
5. Для каждого студента ФИО и названия дисциплин
    1. Которые у него есть по плану (*StudentName*, *CourseName*).  
    2. Есть, но у него нет оценки (*StudentName*, *CourseName*).  
    3. Есть, но у него не 4 или 5 (*StudentName*, *CourseName*).  
    4. Вёл преподаватель (*StudentName*, *CourseName* по *:LecturerName*).  
    5. Вёл преподаватель с *:FacultyName* (*StudentName*, *CourseName* по *:FacultyName*).  
    6. Вёл преподаватель другого факультета (*StudentName*, *CourseName*).  
    7. Вёл декан (*StudentName*, *CourseName*).
6. Идентификаторы студентов по преподавателю
   1. Имеющих хотя бы одну оценку у преподавателя (*StudentId* по *:LecturerName*).
   2. Не имеющих ни одной оценки у преподавателя (*StudentId* по *:LecturerName*).
   3. Имеющих оценки по всем дисциплинам преподавателя (*StudentId* по *:LecturerName*).
   4. Имеющих оценки по всем дисциплинам преподавателя, которые он вёл у этого студента (*StudentId* по *:LecturerName*).
7. Группы и дисциплины, такие что все студенты группы сдали эту дисциплину
   1. Идентификаторы (*GroupId*, *CourseId*).
   2. Названия (*GroupName*, *CourseName*).

Составьте SQL-запросы, позволяющие получать

8. Суммарный балл
   1. Одного студента (*SumMark* по *:StudentId*).
   2. Каждого студента (*StudentName*, *SumMark*).
   3. Каждой группы (*GroupName*, *SumMark*).
9. Средний балл
   1. Одного студента (*AvgMark* по *:StudentId*).
   2. Каждого студента (*StudentName*, *AvgMark*).
   3. Каждой группы (*GroupName*, *AvgMark*).
   4. Средний балл средних баллов студентов каждой группы (*GroupName*, *AvgAvgMark*).
10. Для каждого студента: число дисциплин, которые у него были, число сданных дисциплин и число несданных дисциплин (*StudentId*, *Total*, *Passed*, *Failed*).
