## Домашнее задание 4. Нормализация БД «Университет»

Дано отношение с атрибутами *StudentId*, *StudentName*, *GroupId*, *GroupName*, *GroupFacultyId*, *GroupFacultyName*, *GroupFacultyDeanId*; *CourseId*, *CourseName*, *LecturerId*, *LecturerName*, *LecturerFacultyId*, *LecturerFacultyName*, *LecturerFacultyDeanId*, *Mark*. и функциональными зависимостями:

- StudentId → StudentName, GroupId, GroupName;
- GroupId → GroupName, GroupFacultyId;
- GroupName → GroupId;
- CourseId → CourseName;
- LecturerId → LecturerName, LecturerFacultyId;
- StudentId, CourseId → Mark;
- GroupId, CourseId → LecturerId;
- GroupFacultyId → GroupFacultyName, GroupFacultyDeanId;
- GroupFacultyName → GroupFacultyId;
- LecturerFacultyId → LecturerFacultyName, LecturerFacultyDeanId;
- LecturerFacultyName → LecturerFacultyId.

1. Инкрементально приведите данное отношение в пятую нормальную форму.
2. Постройте соответствующую модель сущность-связь.
3. Постройте соответствующую физическую модель.
4. Реализуйте SQL-скрипты, создающие схему базы данных.
5. Создайте базу данных по спроектированной модели.
6. Заполните базу тестовыми данными.

