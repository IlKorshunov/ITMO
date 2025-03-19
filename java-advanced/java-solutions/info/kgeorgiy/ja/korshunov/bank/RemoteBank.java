package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.RemoteException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private ConcurrentMap<String, Person> pas2pers = new ConcurrentHashMap<>();
    private final int port;

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Person getPerson(String passport, Class<? extends Person> personClass) throws RemoteException {
        Person person = pas2pers.get(passport);
        return personClass.cast(person);
    }
    @Override
    public void AddPerson(String name, String surname, String passport) throws RemoteException {
        pas2pers.putIfAbsent(passport, new RemotePerson(name, surname, passport, this.port));
    }

}
