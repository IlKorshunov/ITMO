package info.kgeorgiy.ja.korshunov.bank;

import info.kgeorgiy.ja.korshunov.bank.Account;

import java.rmi.RemoteException;

public class AbstractAccount implements Account {
    private final String id;
    private Integer amount;
    @Override
    public String getId() throws RemoteException {
        return id;
    }

    @Override
    public int getAmount() throws RemoteException {
        return amount;
    }

    @Override
    public void setAmount(int amount) throws RemoteException {
        this.amount += amount;
    }

    public AbstractAccount(String id){
        this.id = id;
        this.amount = 0;
    }
}
