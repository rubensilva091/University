#! /bin/bash
python3 moonshot/manage.py migrate

if [ $SEED = true ]; then
    python3 seed.py
fi

python3 moonshot/manage.py runserver 0.0.0.0:8000
