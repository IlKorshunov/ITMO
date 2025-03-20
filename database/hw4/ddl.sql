CREATE TABLE Student (
   Id int GENERATED ALWAYS AS IDENTITY primary key,
   Name varchar(25) not null,
   GroupId int not null,
   FOREIGN KEY (GroupId) REFERENCES Groups(Id)
);

CREATE TABLE Groups (
   Id int GENERATED ALWAYS AS IDENTITY primary key,
	group_name varchar(8) not null,
	unique (group_name)
)

CREATE TABLE Course {
    Id int GENERATED ALWAYS AS IDENTITY primary key,
    Name varchar(10) not null,
}

CREATE TABLE Lecturer {
    Id int GENERATED ALWAYS AS IDENTITY primary key,
    Name varchar(25) not null,
}

CREATE TABLE Mark {
    CourseId int not null,
    StudentId int not null,
    Mark int not null,
    PRIMARY KEY (CourseId, StudentId),

    FOREIGN KEY (CourseId) REFERENCES Course(Id),
    FOREIGN KEY (StudentId) REFERENCES Student(Id)
}

CREATE TABLE WorkLecturer {
    GroupId int not null,
    CourseId int not null,
    LecturerId int not null,
    PRIMARY KEY (GroupId, CourseId),

    FOREIGN KEY (CourseId) REFERENCES Course(Id),
    FOREIGN KEY (GroupId) REFERENCES Groups(Id)
}
