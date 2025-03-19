package info.kgeorgiy.ja.korshunov.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {
    // to eliminate copy paste
    private <T> List<T> getStudentInfo(List<Student> students, Function<Student, T> function) {
        return students.stream()
                .map(function)
                .collect(Collectors.toList());
    }
    
    private <T> List<Student> findStudentInfo(Collection<Student> students, Function<Student, T> function, Comparator<Student> comparator, Predicate<T> predicate) {
        return students.stream()
                .filter(student -> predicate.test(function.apply(student)))
                .sorted(comparator)
                .collect(Collectors.toList());
    }

    // interface methods
    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getStudentInfo(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return getStudentInfo(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getStudentInfo(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return getStudentInfo(students,
                student -> student.getFirstName().concat(" ").concat(student.getLastName()));
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return students.stream()
                .sorted(Comparator.comparing(Student::getFirstName))
                .map(Student::getFirstName)
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream()
                .max(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName)
                .orElse(null);
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return findStudentInfo(students, Student::getGroup, Comparator.naturalOrder(), student -> true);
    }


    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return findStudentInfo(students,Student::getFirstName, Comparator.comparing(Student::getFirstName), student -> true);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentInfo(students, Student::getFirstName, Comparator.comparing(Student::getLastName), firstName -> firstName.equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentInfo(students, Student::getLastName, Comparator.comparing(Student::getFirstName), lastName -> lastName.equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentInfo(students, Student::getGroup,
                Comparator.comparing(Student::getFirstName).thenComparing(Student::getLastName), groupObj -> groupObj.equals(group));
    }


    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return students.stream()
                .filter(student -> student.getGroup().equals(group))
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(Comparator.naturalOrder())));
    }
}
