library(marked)
data(dipper)
model = crm(dipper)

model=cjs.hessian(model)
model


dipper.proc=process.data(dipper)
dipper.ddl=make.design.data(dipper.proc)

fit.models=function(){
    Phi.sex=list(formula=~sex)
    Phi.time=list(formula=~time)
    p.sex=list(formula=~sex)
    p.dot=list(formula=~1)
    cml=create.model.list(c("Phi","p"))
    results=crm.wrapper(cml,data=dipper.proc, ddl=dipper.ddl,
                        external=FALSE,accumulate=FALSE)
    return(results)
    }

dipper.models=fit.models()
