package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public abstract class AbstractPerson implements Person {
    private final String name;
    private final String surnname;
    private final String passport;
    private Map<String, Account> accountMap;

    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    @Override
    public Account createAccount(final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        final Account account = new AbstractAccount(id);
        if (accounts.putIfAbsent(id, account) == null) {
            return account;
        } else {
            return getAccount(id);
        }
    }

    @Override
    public Account getAccount(final String id) {
        System.out.println("Retrieving account " + id);
        return accounts.get(GetSubId(id));
    }

    @Override
    public String GetName() throws RemoteException {
        return name;
    }

    @Override
    public String GetSurname() throws RemoteException {
        return surnname;
    }

    @Override
    public String GetPassport() throws RemoteException {
        return passport;
    }

    public String GetSubId(String subId) {
        return this.passport + ":" + subId;
    }


    public AbstractPerson(String name, String surname, String passport){
        this.name = name;
        this.surnname = surname;
        this.passport = passport;
        this.accountMap = new HashMap<>(); // ???
    }
}
