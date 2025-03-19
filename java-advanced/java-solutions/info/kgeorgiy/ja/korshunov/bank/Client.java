package info.kgeorgiy.ja.korshunov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Objects;

public final class Client {
    /** Utility class. */
    private Client() {}

    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
                bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        String name = args[0];
        String surname = args[1];
        String passport = args[2];
        String account_num = args[3];
        String delta = args[4];

        Person getPers = bank.getPerson(passport, RemotePerson.class);
        if (getPers == null){
            bank.AddPerson(name, surname, passport);
        } else {
            if (!Objects.equals(name, getPers.GetName()) || !Objects.equals(surname, getPers.GetSurname())) {
                System.out.println("incorrect name or surname");
            }
        }
        assert getPers != null;
        Account curAcc = getPers.getAccount(account_num);
        System.out.println("name : " + name + "surname : " + surname + "passport : " + passport);
        System.out.println("account : " + curAcc);
        System.out.println("cur amount : " + curAcc.getAmount());
        System.out.println("add amount" + delta);
        curAcc.setAmount(Integer.parseInt(delta));
        System.out.println("after set amount :" + curAcc.getAmount());
    }
}
