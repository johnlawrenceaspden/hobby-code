from django.shortcuts import render

# Create your views here.
from django.http import HttpResponse
import datetime


def index(request):
    return HttpResponse(
        "Hello, World!"
        "<a href='secret'>secrets</a>"
        "<a href='geeks_view'>geeks_view</a>"
        "<a href='template'>template</a>"
    )


def secret(request):
    return HttpResponse("Secrets!")


def geeks_view(request):
    now = datetime.datetime.now()
    html = "Hello, World<br/> time is {} <br/> ".format(now)
    return HttpResponse(html)


def template_view(request):
    return render(request, "template_view.html")


# <a href="{% url 'secret' %}">secrets</a>
